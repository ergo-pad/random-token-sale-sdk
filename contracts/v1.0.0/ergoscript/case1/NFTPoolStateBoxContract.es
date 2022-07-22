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
            val buyerPKBoxesOUT: Box = OUTPUTS(1, OUTPUTS.size-2)
            val txOperatorBox: Box = OUTPUTS(OUTPUTS.size-2)
            val minerBox: Box = OUTPUTS(OUTPUTS.size-1)

            val valid_WhitelistDistributionTx: Boolean = {
                
                val valid_NFTPoolSateBoxReplication: Boolean = {

                    val valid_Box: Boolean = (SELF == nftPoolBoolBoxIN)

                    val valid_Value: Boolean = {
                        (nftPoolStateBoxOUT.value == SELF.value)
                    }

                    val valid_Contract: Boolean = {
                        (nftPoolStateBoxOUT.propositionBytes == SELF.propositionBytes)
                    }

                    val valid_Tokens: Boolean = {
                        
                        val valid_NFTPoolToken: Boolean = {
                            (nftPoolStateBoxOUT.tokens(0) == SELF.tokens(0))
                        }

                        val valid_WhitelistToken: Boolean = {

                            val valid_WhitelistTokenID: Boolean = (nftPoolStateBoxOUT.tokens(1)._1 == SELF.tokens(1)._1)
                            
                            val valid_WhitelistTokenAmount: Boolean = {

                                if (IsWhitelistAirdrop) {

                                    (nftPoolStateBoxOUT.tokens(1)._2 == 1L)
                                
                                } else if (IsWhitelistDirectSale) {
                                    
                                    allOf(Coll(
                                        (nftPoolStateBoxOUT.tokens(1)._2 >= 1),
                                        (nftPoolStateBoxOUT.tokens(1)._2 == SELF.tokens(1)._2 - 1L)
                                    ))
                                
                                } else {
                                    true
                                }

                            }

                            allOf(Coll(
                                valid_WhitelistTokenID,
                                valid_WhitelistTokenAmount
                            ))

                        }

                    }

                    val valid_Registers: Boolean = {

                        val valid_MintAddress: Boolean = (nftPoolStateBoxOUT.R4[Coll[Byte]].get == SELF.R4[Coll[Byte]].get)
                        
                        val valid_NFTRaritiesAndAmounts: Boolean = (nftPoolStateBoxOUT.R5[Coll[(Byte, Int)]].get == SELF.R5[Coll[(Byte, Int)]].get)
                        
                        val valid_TokenRarityPackSizes: Boolean = (nftPoolStateBoxOUT.R6[Coll[Byte]].get == SELF.R6[Coll[Byte]].get)

                        val valid_TotalNFTAmount: Boolean = (nftPoolStateBoxOUT.R7[Long].get == SELF.R7[Long].get)
                        
                        val valid_RemainingNFTAmount: Boolean = (nftPoolStateBoxOUT.R8[Long].get == SELF.R8[Long].get)

                        allOf(Coll(
                            valid_MintAddress,
                            valid_NFTRaritiesAndAmounts,
                            valid_TokenRarityPackSizes,
                            valid_TotalNFTAmount,
                            valid_RemainingNFTAmount
                        ))

                    }

                    allOf(Coll(
                        valid_Box,
                        valid_Value,
                        valid_Contract,
                        valid_Tokens,
                        valid_Registers
                    ))

                }

                val valid_WhitelistTokenDistribution: Boolean = (buyerPKBoxesOUT.forall({ buyerPKBox: Box => buyerPKBox.tokens(0) == (SELF.tokens(1)._1, 1L) }))

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

        val valid_NFTSaleTx: Boolean = {

            val valid_NFTPoolSateBoxReplication: Boolean = {

                val valid_Box: Boolean = (SELF == nftPoolStateBoxIN)

                val valid_Value: Boolean = {
                    (nftPoolStateBoxOUT.value == SELF.value) // must be 2 * MinERGForExistance in case of token retrieval instead of token burn for sale end tx
                }

                val valid_Contract: Boolean = {
                    allOf(Coll(
                        (nftPoolStateBoxOUT.propositionBytes == SELF.propositionBytes)
                    ))
                }

                val valid_Tokens: Boolean = {
                    
                    val valid_NFTPoolNFT: Boolean = (nftPoolStateBoxOUT.tokens(0) == SELF.tokens(0))

                    val valid_WhitelistNFT: Boolean = {

                        if (IsWhitelistNFT) { 
                            (nftPoolStateBoxOUT.tokens(1) == SELF.tokens(1))
                        } else true
                    
                    }

                    allOf(Coll(
                        valid_NFTPoolNFT,
                        valid_WhitelistNFT
                    ))

                }

                val valid_Registers: Boolean = {

                    val valid_MintAddress: Boolean = (nftPoolSateBoxOUT.R4[Coll[Byte]].get == SELF.R4[Coll[Byte]].get)

                    val valid_NFTRaritiesAndAmounts: Boolean = (nftPoolStateBoxOUT.R5[Coll[(Byte, Int)]].get == SELF.R5[Coll[(Byte, Int)]].get)

                    val valid_TokenRarityPackSizes: Boolean = (nftPoolStateBoxOUT.R6[Coll[Byte]].get == SELF.R6[Coll[Byte]].get)
                    
                    val valid_TotalNFTAmount: Boolean = (nftPoolStateBoxOUT.R7[Long].get == SELF.R7[Long].get)

                    val valid_RemainingNFTAmount: Boolean = {
                        allOf(Coll(
                            (remainingNFTAmount >= 0),
                            (nftPoolStateBoxOUT.R8[Long].get == SELF.R8[Long].get - 1)
                        ))
                    }

                }

                allOf(Coll(
                    valid_Box,
                    valid_Value,
                    valid_Contract,
                    valid_Tokens,
                    valid_Registers
                ))

            }

        }

        sigmaProp(valid_NFTSaleTx)

    } else if (isNFTSaleEndTx) {

        val valid_NFTSaleEndTx: Boolean = {

            

        }

        sigmaProp(valid_NFTSaleEndTx)

    } else {
        sigmaProp(false)
    }

}