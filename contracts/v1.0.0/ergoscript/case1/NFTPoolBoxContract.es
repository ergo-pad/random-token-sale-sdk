{
    
    // ===== Contract Information ===== //
    // Name: NFT Pool Box Contract
    // Description: Contract that governs the NFT pool boxes.
    // Version: 1.0.0
    // Author: Luca D'Angelo

    // ===== NFT Sale Tx ===== //
    // Description: Sell one random NFT from the token pool to the user.
    // DataInputs: ErgoPadRNGOracleBox
    // Inputs: NFTPoolStateBox, NFTPoolBoxes, NFTSaleProxyBox
    // Context Extension Variables: None
    // Outputs: NFTPoolStateBox, NFTPoolBoxes, BuyerPKBox, TxOperatorBox

    // ===== NFT Sale End Tx ===== //
    // Description: The sale period has expired and all input tokens are either returned to the sale initiater or are burned.
    // DataInputs: None
    // Inputs: NFTPoolBoxes
    // Context Extension Variables: None
    // Outputs: 
        // With Token Burn: TxOperatorBox
        // Without Token Burn: UserPKBox, TxOperatorBox

    // ===== Hard-Coded Constants ===== //
    val NFTPoolNFT: Coll[Byte] = _NFTPoolNFT
    val BlockHeightLimit: Int = _BlockHeightLimit
    val NFTPoolStateBoxContractHash: Coll[Byte] = blake2b256(_NFTPoolStateBoxContract)
    val IsTokenBurn: Boolean = _IsTokenBurn
    val TxOperatorPK: Coll[Byte] = _TxOperatorPK

    // ===== Spending Path Check ===== //
    val isNFTSaleTx: Boolean = (INPUTS.size >= 4 && OUTPUTS.exists({ output: Box => output.propositionBytes == SELF.propositionBytes }))
    val isNFTSaleEndTx: Boolean = (!OUTPUTS.exists({ output: Box => output.propositionBytes != SELF.propositionBytes }))
    
    if (isNFTSaleTx) {

        // ===== Data Inputs ===== //
        val ergopadRNGOracleBox: Box = dataInputs(0)  // ErgoPad oracle containing random number from drand.love network

        // ===== Inputs ===== //
        val nftPoolStateBoxIN: Box = INPUTS(0)                          // Pool state box containing information about the NFT sale
        val nftPoolBoxesIN: Coll[Box] = INPUTS.slice(1, INPUTS.size-1)  // Boxes containing the different NFTs
        val nftSaleProxyBoxIN: Box = INPUTS(INPUTS.size-1)              // Proxy box containing the buyer's funds and/or their whitelist token

        // ===== Outputs ===== //
        val nftPoolStateBoxOUT: Box = OUTPUTS(0)
        val nftPoolBoxesOUT: Coll[Box] = OUTPUTS.slice(1, OUTPUTS,size-3)  
        val buyerPKBoxOUT: Box = OUTPUTS(OUTPUTS.size-3)
        val txOperatorBoxOUT: Box = OUTPUTS(OUTPUTS.size-2)                
        val minerBox: Box = OUTPUTS(OUTPUTS.size-1)

        // Check conditions for a valid NFT sale transaction
        val valid_NFTSaleTx: Boolean = {

            // Check that the sale period has not expired
            val valid_TimeRemaining: Boolean = {

                val currentBlockHeight: Int = CONTEXT.preHeader.height

                (currentBlockHeight <= BlockHeightLimit)

            }

            val valid_NFTPoolStateBoxIN: Boolean = {

                allOf(Coll(
                    (blake2b256(nftPoolStateBoxIN.propositionBytes) == NFTPoolStateBoxContractHash),
                    (nftPoolStateBoxIN.tokens(0) == (NFTPoolNFT, 1L))
                ))

            }
        
            val valid_NFTRemoval: Boolean = {
                
                val totalNFTInOutputPool: Int = nftPoolBoxesOUT.fold(0.toInt, { (acc: Long, nftPoolBoxOUT: Box) => nftPoolBoxesOUT.tokens.size + acc })
                
                allOf(Coll(
                    (remainingNFTAmount > 0),
                    (totalNFTInOutputPool == remainingNFTAmount - 1) 
                ))
            
            }

            allOf(Coll(
                valid_TimeRemaining,
                valid_NFTPoolStateBoxIN,
                valid_NFTRemoval
            ))

        }

        sigmaProp(valid_NFTSaleTx)

    } else if (isNFTSaleEndTx) {

        // ===== Inputs ===== //
        val nftPoolStateBoxIN: Box = INPUTS(0)
        val nftPoolBoxesIN: Coll[Box] = INPUTS.slice(1, INPUTS.size)

        // ===== Outputs ===== //
        val userPKBoxesOUT: Coll[Box] = if (!IsTokenBurn) OUTPUTS.slice(0, OUTPUTS.size-2) else OUTPUTS
        val txOperatorBox: Box = OUTPUTS(OUTPUTS.size-2)
        val minerBox: Box = OUTPUTS(OUTPUTS.size-1)

        // Check conditions for a valid sale end transaction, which occurs if not all NFTs in the pool have been sold after the sale period has expired.
        val valid_NFTSaleEndTx: Boolean = {

            val valid_TimeExpired: Boolean = {

                val currentBlockHeight: Int = CONTEXT.preHeader.height

                (currentBlockHeight > BlockHeightLimit)

            }

            val valid_NFTPoolSateBoxIN: Boolean = {
                allOf(Coll(
                    (blake2b256(nftPoolStateBoxIN.propositionBytes) == NFTPoolStateBoxContractHash),
                    (nftPoolStateBoxIN.tokens(0) == (NFTPoolNFT, 1L))
                ))
            }

            allOf(Coll(
                valid_TimeExpired,
                valid_NFTPoolSateBoxIN
            ))

        }

        sigmaProp(valid_NFTSaleEndTx)

    } else {
        sigmaProp(false)
    }
   
}