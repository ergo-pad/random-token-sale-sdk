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

    // ===== Sale End Tx ===== //
    // Description: The sale period has expired and all tokens must be burned
    // DataInputs: None
    // Inputs: NFTPoolBoxes
    // Context Extension Variables: None
    // Outputs: WithdrawBox, MinerBox

    // ===== Hard-Coded Constants ===== //
    val MintAddress: Coll[Byte] = _MintAddress
    val NFTPoolNFT: Coll[Byte] = _NFTPoolNFT
    val Rarity: Byte = _Rarity
    val MinERGForExistance: Long = 1000000L
    val BlockHeightLimit: Int = _BlockHeightLimit
    val MinerFee: Long = MinERGForExistance

    // ===== Spending Path Check ===== //
    val isNFTSaleTx: Boolean = (INPUTS.size > 1)
    val isSaleEnd: Boolean = (OUTPUTS.size == 2)
    
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
        
            val valid_NFTPoolBoxReplication: Boolean = {

                // ===== Relevant Variables ===== //
                val boxIndices: Coll[Int] = if (nftPoolBoxesIN.size == nftPoolBoxesOUT.size) nftPoolBoxesIN.indices else nftPoolBoxesIN.indices.slice(0, nftPoolBoxesIN.indices.size-1)
                val prefix: Coll[Byte] = Coll(0.toByte, 0.toByte, 0.toByte, 0.toByte)

                val valid_Inputs: Boolean = {

                    val nftPoolBoxesINANDInputIndices: Coll[(Box, Int)] = nftPoolBoxesIN.zip(boxIndices)
                    
                    nftPoolBoxesINANDInputIndices.forall({ nftPoolBoxINANDBoxIndex: (Box, Int) =>

                        val nftPoolBoxIN: Box = nftPoolBoxINANDBoxIndex._1
                        val boxIndex: Int = nftPoolBoxINANDBoxIndex._2
                
                        val valid_Value: Boolean = (nftPoolBoxIN.value == MinERGForExistance * 2.toLong)
                        
                        val valid_Contract: Boolean = (nftPoolBoxIN.propositionBytes == SELF.propositionBytes)

                        val valid_NFTPoolToken: Boolean = (nftPoolBoxIN.tokens(0) == (NFTPoolNFT, 1L))

                        // Check that the input indices of the pool boxes remain constant from each NFT sale tx to the next
                        val valid_InputIndex: Boolean = {

                            val nftPoolBoxINIndexBytes: Coll[Byte] = nftPoolBoxIN.creationInfo._2.slice(32, 36)
                            val nftPoolBoxINIndex: Int = byteArrayToLong(prefix.append(nftPoolBoxINIndexBytes)).toInt

                            (nftPoolBoxINIndex == boxIndex)

                        }

                        val valid_MintAddress: Boolean = (nftPoolBoxIN.R4[Coll[Byte]].get == MintAddress)

                        val valid_Rarity: Boolean = (nftPoolBoxIN.R5[Byte] == Rarity)

                        allOf(Coll(
                            valid_Value,
                            valid_Contract,
                            valid_NFTPoolToken,
                            valid_InputIndex,
                            valid_MintAddress,
                            valid_Rarity
                        ))

                    
                    })

                }

                val valid_Outputs: Boolean = {

                    val nftPoolBoxesOUTANDBoxIndices: Coll[(Box, Int)] = nftPoolBoxesOUT.zip(boxIndices)

                    nftPoolBoxesOUTANDBoxIndicies.forall({ nftPoolBoxOUTANDBoxIndex: (Box, Int) => 
                
                        val nftPoolBoxesOUT: Box = nftPoolBoxOUTANDBoxIndex._1
                        val boxIndex: Int = nftPoolBoxOUTANDBoxIndex._2

                        val valid_Value: Boolean = (nftPoolBoxOUT.value == MinERGForExistance * 2.toLong)

                        val valid_Contract: Boolean = (nftPoolBoxOUT.propositionBytes == SELF.propositionBytes)

                        val valid_NFTPoolToken: Boolean = (nftPoolBoxOUT.tokens(0) == (NFTPoolNFT, 1L))

                        // Check that the output indices of the pool boxes remain constant from each NFT sale tx to the next
                        val valid_OutputIndex: Boolean = {
                            
                            val nftPoolBoxOUTIndexBytes: Coll[Byte] = nftPoolBoxOUT.creationInfo._2.slice(32, 36)
                            val nftPoolBoxOUTIndex: Int = byteArrayToLong(prefix.append(nftPoolBoxOUTIndexBytes)).toInt

                            (nftPoolBoxOUTIndex == boxIndex) 

                        }

                        val valid_MintAddress: Boolean = (nftPoolBoxIN.R4[Coll[Byte]].get == MintAddress)

                        val valid_Rarity: Boolean = (nftPoolBoxOUT.R5[Byte] == Rarity)

                        allOf(Coll(
                            valid_Value,
                            valid_Contract,
                            valid_NFTPoolToken,
                            valid_OutputIndex,
                            valid_MintAddress,
                            valid_Rarity
                        ))

                    })

                }

                val valid_NFTRemoval: Boolean = {
                    
                    val totalTokensInOutputPool: Int = nftPoolBoxesOUT.fold(0.toInt, { (acc: Long, nftPoolBoxOUT: Box) => nftPoolBoxesOUT.tokens.size + acc })
                    
                    allOf(Coll(
                        (remainingNFTAmount > 0),
                        (totalTokensInOutputPool == remainingNFTAmount - 1) 
                    ))
                
                }

                allOf(Coll(
                    valid_Inputs,
                    valid_Outputs,
                    valid_NFTRemoval
                ))

            }

            // Check that the sale period has not expired
            val valid_TimeRemaining: Boolean = {

                val currentBlockHeight: Int = CONTEXT.preHeader.height

                (currentBlockHeight <= BlockHeightLimit)

            }

            allOf(Coll(
                valid_NFTPoolBoxReplication,
                valid_TimeRemaining
            ))

        }

        sigmaProp(valid_NFTSaleTx)

    } else if (isSaleEnd) {

        // ===== Inputs ===== //
        val nftPoolBoxesIN: Coll[Box] = INPUTS

        // ===== Outputs ===== //
        val withdrawBox: Box = OUTPUTS(0)
        val minerBox: Box = OUTPUTS(1)

        // Check conditions for a valid sale end transaction, which occurs if not all NFTs in the pool have been sold after the sale period has expired.
        val valid_SaleEndTx: Boolean = {

            val valid_TimeExpired: Boolean = {

                val currentBlockHeight: Int = CONTEXT.preHeader.height

                (currentBlockHeight > BlockHeightLimit)

            }

            val valid_WithdrawBox: Boolean = {

                val valid_Value: Boolean = {

                    val inputValue: Long = nftPoolBoxesIN.fold(0L, { (acc: Long, nftPoolBoxIN: Box) => nftPoolBoxesIN.value + acc })

                    (withdrawBox.value == inputValue - SaleEndMinerFee)

                }

                val valid_Tokens: Boolean = {

                    // All input tokens are burned
                    (withdrawBox.tokens == Coll[(Coll[Byte], Long)]())

                }

                allOf(Coll(
                    valid_Value,
                    valid_Tokens
                ))

            }

            val valid_MinerBox: Boolean = {
                
                val valid_Value: Boolean = {
                    (minerBox.value == MinerFee)
                }

                val valid_Tokens: Boolean = {
                    (minerBox.tokens == Coll[(Coll[Byte], Long)]())
                }

                allOf(Coll(
                    valid_Value,
                    valid_Tokens
                ))

            }


            allOf(Coll(
                valid_TimeExpired,
                valid_WithdrawBox,
                valid_MinerBox
            ))

        }

        sigmaProp(valid_SaleEndTx)

    } else {
        sigmaProp(false)
    }
   
}