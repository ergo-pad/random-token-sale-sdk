{

    // ===== Contract Information ===== //
    // Name: NFT Pool State Box Contract
    // Description: Contract that governs the NFT pool state box.
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

    // ===== NFT Sale Tx ===== //
    // Description: Selling random NFT(s) from the token pool to the user.
    // DataInputs: None
    // Inputs: ErgoPadRNGOracleBox, NFTPoolStateBox, NFTPoolBoxes, NFTSaleProxyBox
    // Context Extension Variables: None
    // Outputs: NFTPoolSateBox, NFTPoolBoxes, BuyerPKBox, TxOperatorBox

    // ===== Sale End Tx ===== //
    // Description: The sale period has expired and all input tokens are either returned to the sale initiater or are burned.
    // DataInputs: None
    // Inputs: NFTPoolBoxes
    // Context Extension Variables: None
    // Outputs: 
        // With Token Burn: TxOperatorBox
        // Without Token Burn: UserPKBox, TxOperatorBox

    // ===== Hard-Coded Constants ===== //
    val MintAddress: Coll[Byte] = _MintAddress
    val TxOperatorPK: Coll[Byte] = _TxOperatorPK
    val IsWhitelistAirdrop: Boolean = _IsAirdrop
    val IsWhitelistDirectSale: Boolean = _IsWhitelistAirdrop
    val IsWhitelistNFT: Boolean = (SELF.tokens.size == 2)

    // ===== Spending Path Check ===== //
    val isWhitelistDistributionTx: Boolean = (OUTPUTS.exists({ output: Box => output.propositionBytes == SELF.propositionBytes }))
    val isNFTSaleTx: Boolean = (INPUTS.size >= 5 && OUTPUTS.exists({ output: Box => output.propositionBytes == SELF.propositionBytes }))
    val isNFTSaleEndTx: Boolean = (!OUTPUTS.exists({ output: Box => output.propositionBytes != SELF.propositionBytes }))
       
    if (isWhitelistDistributionTx) {

        if (IsWhitelistNFT) {

            // ===== Inputs ===== //
            val nftPoolStateBoxIN: Box = INPUTS(0)
            val whitelistProxyBoxIN: Box = INPUTS(1)

            // ===== Outputs ===== //
            val nftPoolStateBoxOUT: Box = OUTPUTS(0)
            val buyerPKBoxes: Box = OUTPUTS(1, OUTPUTS.size-2)
            val txOperatorBox: Box = OUTPUTS(OUTPUTS.size-2)
            val minerBox: Box = OUTPUTS(OUTPUTS.size-1)

            val valid_WhitelistDistributionTx: Boolean = {
                
                val valid_NFTPoolSateBoxReplication: Boolean = {

                    val valid_Value: Boolean = {
                        (nftPoolStateBoxIN.value == nftPoolStateBoxOUT.value)
                    }

                    val valid_Contract: Boolean = {
                        (nftPoolStateBoxIN.value == nftPoolStateBoxOUT.value)
                    }

                    val valid_Tokens: Boolean = {
                        
                        val valid_NFTPoolToken: Boolean = {
                            (nftPoolStateBoxIN.tokens(0) == nftPoolStateBoxOUT.tokens(0))
                        }

                        val valid_WhitelistToken: Boolean = {

                            val valid_WhitelistTokenID: Boolean = (nftPoolStateBoxIN.tokens(1)._1 == nftPoolStateBoxOUT.tokens(1)._1)
                            
                            val valid_WhitelistTokenAmount: Boolean = {

                                if (IsWhitelistAirdrop) {
                                    (nftPoolStateBoxOUT.tokens(1)._2 = 1L)
                                } else if (IsWhitelistDirectSale) {
                                    (nftPoolStateBoxO)
                                }

                            }

                            allOf(Coll(
                                valid_WhitelistTokenID,
                                valid_WhitelistTokenAmount
                            ))

                        }

                    }

                    val valid_Registers: Boolean = {



                    }

                }

                val valid_WhitelistTokenDistribution: Boolean = {



                }

                allOf(Coll(
                    valid_NFTPoolSateBoxReplication,
                    valid_WhitelistTokenDistribution
                ))
                
            }

            sigmaProp(valid_WhitelistDistributionTx)

        } else {
            false
        }

    } else if (isNFTSaleTx) {



    } else if (isNFTSaleEndTx) {



    } else {
        sigmaProp(false)
    }

}