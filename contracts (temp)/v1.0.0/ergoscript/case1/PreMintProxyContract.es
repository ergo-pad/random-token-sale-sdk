{

    // ===== Contract Information ===== //
    // Name: Pre-Mint Proxy Contract
    // Description: Proxy contract guarding the user's funds used for the minting of their NFT pool.
    // Version: 1.0.0
    // Author: Luca D'Angelo

    // ===== Pre-Mint Tx ===== //
    // Description: User sends funds to this proxy contract and the minting process begins.
    // DataInputs: None
    // Inputs: PreMintProxyBox
    // Context Extension Variables: None
    // Outputs: NFTIssuerProxyBoxes, TxOperatorBox

    // ===== Refund Tx ===== //
    // Description: User has sent funds to the pre-mint proxy but wants to cancel the minting process and retreive their funds back.
    // DataInputs: None
    // Inputs: PreMintProxyBox
    // Context Extension Variables: None
    // Outputs: UserPKBox, TxOperatorBox

    // ===== Hard-Coded Constants ===== //
    val MintAddress: Coll[Byte] = _MintAddress
    val IsWhitelist: Boolean = _IsWhitelist
    val MinERGForExistance: Long = 1000000
    val MintingFee: Long = _MintingFee
    val TxOperatorFee: Long = _TxOperatorFee
    val MinerFee: Long = MinERGForExistance
    val TxOperatorPK: Coll[Byte] = _TxOperatorPK
    val NFTIssuerProxyContractHash: Coll[Byte] = blake2b256(_NFTIssuerProxyContract)
    val NFTPoolIssuerProxyContractHash: Coll[Byte] = blake2b256(_NFTPoolIssuerContract)
    val WhitelistIssuerProxyContractHash: Coll[Byte] = blake2b256(_WhitelistIssuerProxyContract)
    val NFTRoyaltyPercentages: Coll[Int] = _NFTRoyaltyPercentages

    // ===== Spending Path Check ===== //
    val isPreMintTx: Boolean = (OUTPUTS(0).propositionBytes != MintAddress)
    val isRefundTx: Boolean = (OUTPUTS(0).propositionBytes == MintAddress)

    if (isPreMintTx) {

        // ===== Inputs ===== //
        val preMintProxyBoxIN: Box = INPUTS(0)

        // ===== Outputs ===== //
        val nftIssuerProxyBoxesOUT: Coll[Box] = if (IsWhitelist) OUTPUTS.slice(0, OUTPUTS.size-4) else OUTPUTS.slice(0, OUTPUTS.size-3)
        val nftPoolIssuerProxyBoxOUT: Box = if (IsWhitelist) OUTPUTS(OUTPUTS.size-4) else OUTPUTS(OUTPUTS.size-3)
        val whitelistIssuerProxyBoxOUT: Box = if (IsWhitelist) OUTPUTS(OUTPUTS.size-3) else OUTPUTS(0)
        val txOperatorBox: Box = OUTPUTS(OUTPUTS.size-2)
        val minerBox: Box = OUTPUTS(OUTPUTS.size-1)

        val valid_PreMintTx: Boolean = {

            val validMintingCost: Boolean = {

                if (IsWhitelist) {
                    (SELF.value >= MintingFee + ( (MinERGForExistance * 2) * (nftIssuerProxyBoxesOUT.size + 2) ) + TxOperatorFee + MinerFee)
                } else {
                    (SELF.value >= MintingFee + ( (MinERGForExistance * 2) * (nftIssuerProxyBoxesOUT.size + 1) ) + TxOperatorFee + MinerFee)
                }

            }

            val validNFTIssuerProxyBoxes: Boolean = {

                nftIssuerProxyBoxesOUT.forall({ nftIssuerProxyBox: Box => 
                
                    val validValue: Boolean = (nftIssuerProxyBox.value == 2 * MinERGForExistance)

                    val validContract: Boolean = (blake2b256(nftIssuerProxyBox.propositionBytes) == NFTIssuerProxyContractHash)
                
                    val validRoyaltyPercentage: Boolean = ()

                    allOf(Coll(
                        validValue,
                        validContract
                    ))

                })

            }

            val validNFTPoolIssuerProxyBox: Boolean = {

                val validValue: Boolean = (nftPoolIssuerProxyBoxOUT.value == 2 * MinERGForExistance)

                val validContract: Boolean = (blake2b256(nftPoolIssuerProxyBoxOUT.propositionBytes) == NFTPoolIssuerProxyContractHash)

                allOf(Coll(
                    validValue,
                    validContract
                ))

            }

            val validWhitelistIssuerProxyBox: Boolean = {



            }

            val validTxOperatorBox: Boolean = {

                val validValue: Boolean = (txOperatorBox.value == TxOperatorFee)

                val validPK: Boolean = (txOperatorBox.propositionBytes == TxOperatorPK)

                allOf(Coll(
                    validValue,
                    validPK
                ))

            }

            val validMinerFee: Boolean = (minerBox.value == MinerFee)

        }

        sigmaProp(valid_PreMintTx)

    } else if (isRefundTx) {

        val valid_RefundTx: Boolean = {

            

        }
        
        sigmaProp(valid_RefundTx)

    } else {
        sigmaProp(false)
    }

}