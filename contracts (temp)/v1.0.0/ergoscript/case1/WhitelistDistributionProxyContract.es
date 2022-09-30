{

    // ===== Contract Information ===== //
    // Name: Whitelist Distribution Proxy Contract
    // Description: Contract that governs the whitelist distribution proxy box, used for both whitelist token airdrops and whitelist token direct sales.
    // Version: 1.0.0
    // Author: Luca D'Angelo

    // ===== Whitelist Airdrop Tx ===== //
    // Description: Airdrop the whitelist tokens to user PKs.
    // DataInputs: None
    // Inputs: NFTPoolSateBox, WhitelistAirdropProxyBox
    // Context Extension Variables: None
    // Outputs: NFTPoolStateBox, UserPKBoxes, TxOperatorBox

    // ===== Whitelist Direct Sale Tx ===== //
    // Description: Users purchase the whitelist token directly.
    // DataInputs: None
    // Inputs: NFTPoolStateBox, WhitelistDirectSaleProxyBox 
    // Context Extension Variabels:
    // Outputs: NFTPoolStateBox, BuyerPKBox, TxOperatorBox

    // ===== Hard-Coded Constants ===== //
    val MintAddress: Boolean = _MintAddress
    val IsWhitelistAidrop: Boolean = _IsWhitelistAirdrop
    val IsWhitelistDirectSale: Boolean = _IsWhitelistDirectSale
    val NFTPoolToken: Coll[Byte] = _NFTPoolToken
    val WhitelistToken: Coll[Byte] _WhitelistToken
    val MinERGForExistance: Long = 1000000
    val WhitelistTokenPrice: Boolean = _WhitelistTokenPrice
    val TxOperatorFee: Long = _TxOperatorFee
    val MinerFee: Long = MinERGForExistance
    val UserPK: Coll[Byte] = _UserPK
    val BuyerPKs: Coll[Coll[Byte]] = _BuyerPKs
    val TxOperatorPK: Coll[Byte] = _TxOperatorPK

    // ===== Spending Path Check ===== //
    val isWhitelistDistributionTx: Boolean = (INPUTS.size == 2)
    val isRefundTx: Boolean = (INPUTS.size == 1)

    if (isWhitelistDistributionTx) {

        // ===== Inputs ===== //
        val nftPoolStateBoxIN: Box = INPUTS(0)
        val whitelistDistributionProxyBoxIN: INPUTS(1)

        // ===== Outputs ===== //
        val nftPoolStateBoxOUT: Box = OUTPUTS(0)
        val buyerPKBoxesOUT: Box = OUTPUTS(1, OUTPUTS.size-2)
        val txOperatorBox: Box = OUTPUTS(OUTPUTS.size-2)
        val minerBox: Box = OUTPUTS(OUTPUTS.size-1)

        val validWhitelistDistributionTx: Boolean = {

            val validNFTPoolToken: Boolean = (NFTPoolNFT == nftPoolBoolBoxIN.tokens(0)._1)

            val validWhitelistToken: Boolean = (WhitelistToken == nftPoolBoxIN.tokens(1)._1)

            val validWhitelistTokenSaleCost: Boolean = {
                    
                if (IsWhitelistAidrop) {
                    (SELF.value >= (MinERGForExistance * buyerPKBoxesOUT.size) + WhitelistTokenPrice + TxOperatorFee + MinerFee)
                } else if (IsWhitelistDirectSale) {
                    (SELF.value >= MinERGForExistance + WhitelistTokenPrice + TxOperatorFee + MinerFee)
                } else {
                    false
                }

            }

            val validBuyerPKBoxes: Boolean = {

                buyerPKBoxesOUT.forall({ buyerPKBox: Box => 

                    val validValue: Boolean = (buyerPKBox.value == MinERGForExistance)

                    val validPK: Boolean = {
                        
                        if (IsWhitelistAirdrop) {
                            BuyerPKs.exists({ buyerPK: Coll[Byte] => buyerPK == buyerPKBox.propositionBytes })
                        } else if (IsWhitelistDirectSale) {
                            (buyerPKBox.propositionBytes == BuyerPKs(0))
                        } else {
                            false
                        }
                       
                    }

                    val validWhitelistTokenDistribution: Boolean =  (buyerPKBox.tokens(0) == (WhitelistToken, 1L))

                    allOf(Coll(
                        validValue,
                        validPK,
                        validWhitelistTokenDistribution
                    ))
                
                })

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

            allOf(Coll(
                validNFTPoolToken,
                validWhitelistToken,
                validWhitelistTokenSaleCost,
                validBuyerPKBoxes,
                validTxOperatorBox,
                validMinerFee
            ))

        }

        sigmaProp(validWhitelistDistributionTx)

    } else if (isRefundTx) {

        // ===== Inputs ===== //
        val whitelistDistributionProxyBoxIN: Box = INPUTS(0)

        // ===== Outputs ===== //
        val UserPKBoxOUT: Box = OUTPUTS(0)
        val txOperatorBox: Box = OUTPUTS(1)
        val minerBox: Box = OUTPUTS(2) 

        val validRefundTx: Boolean = {

            val validUserPKBox: Boolean = {

                val validValue: Boolean = (userPKBoxOUT.value == SELF.value - TxOperatorFee - MinerFee)

                val validPK: Boolean = {
                    
                    if (IsWhitelistAirdrop) {
                        (userPKBoxOUT.propositionBytes == UserPK)
                    } else if (IsWhitelistDirectSale) {
                        (userPKBoxOUT.propositionBytes == BuyerPKs(0))
                    } else {
                        false
                    }

                }

                allOf(Coll(
                    validValue,
                    validPK
                ))

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

            allOf(Coll(
                validUserPKBoxOUT,
                validTxOperatorBox,
                validMinerFee
            ))

        }

        sigmaProp(validRefundTx)

    } else {
        sigmaProp(false)
    }





}