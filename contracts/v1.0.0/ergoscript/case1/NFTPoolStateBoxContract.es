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
    val MintAddres: Coll[Byte] = SELF.R4[Coll[Byte]].get
    val IsWhitelist: Boolean = _IsWhitelist
    val IsWhitelistAirdrop: Boolean = _IsWhitelistAirdrop
    val IsWhitelistDirectSale: Boolean = _IsWhitelistDirectSale
    val IsTokenBurn: Boolean = _IsTokenBurn
    val TxOperatorPK: Coll[Byte] = _TxOperatorPK
    val NFTPoolBoxContractHash: Coll[Byte] == blake2b256(_NFTPoolBoxContract)

    // ===== Spending Path Check ===== //
    val isWhitelistDistributionTx: Boolean = (OUTPUTS.exists({ output: Box => output.propositionBytes == SELF.propositionBytes }))
    val isNFTSaleTx: Boolean = (INPUTS.size >= 4 && OUTPUTS.exists({ output: Box => output.propositionBytes == SELF.propositionBytes }))
    val isNFTSaleEndTx: Boolean = (!OUTPUTS.exists({ output: Box => output.propositionBytes != SELF.propositionBytes }))
       
    if (isWhitelistDistributionTx) {

        if (IsWhitelist) {

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

        val valid_NFTSaleTx: Boolean = {

            val valid_NFTPoolStateBoxReplication: Boolean = {

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

                        if (IsWhitelist) { 
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

            val valid_NFTPoolBoxReplication: Boolean = {

                val boxIndices: Coll[Int] = if (nftPoolBoxesIN.size == nftPoolBoxesOUT.size) nftPoolBoxesIN.indices else nftPoolBoxesIN.indices.slice(0, nftPoolBoxesIN.indices.size-1)
                val prefix: Coll[Byte] = Coll(0.toByte, 0.toByte, 0.toByte, 0.toByte)

                val valid_Inputs: Boolean = {

                    val nftPoolBoxesINANDInputIndices: Coll[(Box, Int)] = nftPoolBoxesIN.zip(boxIndices)
                    
                    nftPoolBoxesINANDInputIndices.forall({ nftPoolBoxINANDBoxIndex: (Box, Int) =>

                        val nftPoolBoxIN: Box = nftPoolBoxINANDBoxIndex._1
                        val boxIndex: Int = nftPoolBoxINANDBoxIndex._2
                
                        val valid_Value: Boolean = (nftPoolBoxIN.value == MinERGForExistance)
                        
                        val valid_Contract: Boolean = (blake2b256(nftPoolBoxIN.propositionBytes) == NFTPoolBoxContractHash)

                        val valid_NFTPoolToken: Boolean = (nftPoolBoxIN.tokens(0) == (NFTPoolNFT, 1L))

                        // Check that the input indices of the pool boxes remain constant from each NFT sale tx to the next
                        val valid_InputIndex: Boolean = {

                            val nftPoolBoxINIndexBytes: Coll[Byte] = nftPoolBoxIN.creationInfo._2.slice(32, 36)  // Only the last 8 bytes are needed.
                            val nftPoolBoxINIndex: Int = byteArrayToLong(prefix.append(nftPoolBoxINIndexBytes)).toInt

                            (nftPoolBoxINIndex == boxIndex)

                        }

                        val valid_MintAddress: Boolean = (nftPoolBoxIN.R4[Coll[Byte]].get == MintAddress)

                        allOf(Coll(
                            valid_Value,
                            valid_Contract,
                            valid_NFTPoolToken,
                            valid_InputIndex,
                            valid_MintAddress
                        ))

                    
                    })

                }

                val valid_Outputs: Boolean = {

                    val nftPoolBoxesOUTANDBoxIndices: Coll[(Box, Int)] = nftPoolBoxesOUT.zip(boxIndices)

                    nftPoolBoxesOUTANDBoxIndicies.forall({ nftPoolBoxOUTANDBoxIndex: (Box, Int) => 
                
                        val nftPoolBoxesOUT: Box = nftPoolBoxOUTANDBoxIndex._1
                        val boxIndex: Int = nftPoolBoxOUTANDBoxIndex._2

                        val valid_Value: Boolean = (nftPoolBoxOUT.value == MinERGForExistance)

                        val valid_Contract: Boolean = (blake2b256(nftPoolBoxOUT.propositionBytes) == NFTPoolBoxContractHash)

                        val valid_NFTPoolToken: Boolean = (nftPoolBoxOUT.tokens(0) == (NFTPoolNFT, 1L))

                        // Check that the output indices of the pool boxes remain constant from each NFT sale tx to the next
                        val valid_OutputIndex: Boolean = {
                            
                            val nftPoolBoxOUTIndexBytes: Coll[Byte] = nftPoolBoxOUT.creationInfo._2.slice(32, 36)  // Only the last 8 bytes are needed.
                            val nftPoolBoxOUTIndex: Int = byteArrayToLong(prefix.append(nftPoolBoxOUTIndexBytes)).toInt

                            (nftPoolBoxOUTIndex == boxIndex) 

                        }

                        val valid_MintAddress: Boolean = (nftPoolBoxIN.R4[Coll[Byte]].get == MintAddress)

                        allOf(Coll(
                            valid_Value,
                            valid_Contract,
                            valid_NFTPoolToken,
                            valid_OutputIndex,
                            valid_MintAddress
                        ))

                    })

                }
            }

            allOf(Coll(
                valid_NFTPoolStateBoxReplication
                valid_NFTPoolBoxReplication,
                valid_MintAddress
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

        val valid_NFTSaleEndTx: Boolean = {

            val valid_NFTPoolStateBoxIN: Boolean = (nftPoolStateBoxIN == SELF)

            val valid_NFTPoolBoxesIN: Boolean = {
                
                nftPoolBoxesIN.forall({ nftPoolBoxIN: Box => 
                    
                    val valid_Contract: Boolean = (blake2b256(nftPoolBoxesIN.propositionBytes) == NFTPoolBoxContractHash)
                    
                    val valid_NFTPoolNFT: Boolean = (nftPoolBoxIN.tokens(0) == SELF.tokens(0))

                    allOf(Coll(
                        valid_Contract,
                        valid_NFTPoolNFT
                    ))
                
                })
            
            }

            val valid_UserPKBoxesOUT: Boolean = {
            
                if (!IsTokenBurn) {

                    userPKBoxesOUT.forall({ userPKBoxOUT: Box =>
                    
                        val valid_Value: Boolean = (userPKBoxOUT.value == MinERGForExistance)
                        
                        val valid_PK: Boolean = (userPKBoxOUT.propositionBytes == MintAddres)

                        val valid_Tokens: Boolean = {

                            val nftCollection: Coll[(Coll[Byte], Long)] = INPUTS.fold(Coll[(Coll[Byte], Long)](), { (acc: Coll[(Coll[Byte], Long)], nftPoolBoxIN: Box) => acc ++ nftPoolBoxIN.tokens.slice(1, nftPoolBoxIN.tokens.size) })

                            (userPKBoxOUT.tokens == nftCollection)

                        }

                        allOf(Coll(
                            valid_Value,
                            valid_PK,
                            valid_Tokens
                        ))
                    
                    })
                
                } else {
                    true
                }
                
            }

            val valid_TxOperatorBox: Boolean = {

                val valid_Value: Boolean = {

                    val inputValue: Long = INPUTS.fold(0L, { (acc: Long, input: Box) => input.value + acc })

                    if (IsTokenBurn) {

                        (txOperatorBox.value == inputValue - MinerFee)

                    } else {

                        (txOperatorBox.value == inputValue - MinERGForExistance - MinerFee)

                    }

                }

                val valid_PK: Boolean = (txOperatorBox.propositionBytes == TxOperatorPK)

                val valid_Tokens: Boolean = {
                    
                    if (IsTokenBurn) {

                         // All input tokens must be burned
                        (txOperatorBox.tokens == Coll[(Coll[Byte], Long)]())
                    
                    } else {
                        true
                    }
                
                }

                allOf(Coll(
                    valid_Value,
                    valid_PK,
                    valid_Tokens
                ))

            }

            val valid_MinerBox: Boolean = {
                
                val valid_Value: Boolean = {
                    (minerBox.value == MinerFee)
                }

                // All input tokens must be burned
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
                valid_NFTPoolSateBoxIN,
                valid_NFTPoolBoxesIN,
                valid_UserPKBoxesOUT,
                valid_TxOperatorBox,
                valid_MinerBox
            ))

        }

        sigmaProp(valid_NFTSaleEndTx)

    } else {
        sigmaProp(false)
    }

}