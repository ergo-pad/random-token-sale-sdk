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
    val isWhitelistDistributionTx: Boolean = (INPUTS.size == 2 && OUTPUTS(0).propositionBytes == SELF.propositionBytes)
    val isNFTSaleTx: Boolean = (INPUTS.size >= 4 && OUTPUTS(0).propositionBytes == SELF.propositionBytes)
    val isNFTSaleEndTx: Boolean = (!OUTPUTS.exists({ output: Box => output.propositionBytes != SELF.propositionBytes }))
       
    if (isWhitelistDistributionTx) {

        if (IsWhitelist) {

            // ===== Inputs ===== //
            val nftPoolStateBoxIN: Box = INPUTS(0)
            val whitelistDistributionProxyBoxIN: Box = INPUTS(1)

            // ===== Outputs ===== //
            val nftPoolStateBoxOUT: Box = OUTPUTS(0)
            val buyerPKBoxesOUT: Box = OUTPUTS(1, OUTPUTS.size-2)
            val txOperatorBox: Box = OUTPUTS(OUTPUTS.size-2)
            val minerBox: Box = OUTPUTS(OUTPUTS.size-1)

            val validWhitelistDistributionTx: Boolean = {
                
                val validNFTPoolSateBoxReplication: Boolean = {

                    val validBox: Boolean = (SELF == nftPoolBoolBoxIN)

                    val validValue: Boolean = {
                        (nftPoolStateBoxOUT.value == SELF.value)
                    }

                    val validContract: Boolean = {
                        (nftPoolStateBoxOUT.propositionBytes == SELF.propositionBytes)
                    }

                    val validTokens: Boolean = {
                        
                        val validNFTPoolToken: Boolean = {
                            (nftPoolStateBoxOUT.tokens(0) == SELF.tokens(0))
                        }

                        val validWhitelistToken: Boolean = {

                            val validWhitelistTokenID: Boolean = (nftPoolStateBoxOUT.tokens(1)._1 == SELF.tokens(1)._1)
                            
                            val validWhitelistTokenAmount: Boolean = {

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
                                validWhitelistTokenID,
                                validWhitelistTokenAmount
                            ))

                        }

                    }

                    val validRegisters: Boolean = {

                        val validMintAddress: Boolean = (nftPoolStateBoxOUT.R4[Coll[Byte]].get == SELF.R4[Coll[Byte]].get)
                        
                        val validNFTRaritiesAndAmounts: Boolean = (nftPoolStateBoxOUT.R5[Coll[(Byte, Int)]].get == SELF.R5[Coll[(Byte, Int)]].get)
                        
                        val validTokenRarityPackSizes: Boolean = (nftPoolStateBoxOUT.R6[Coll[Byte]].get == SELF.R6[Coll[Byte]].get)

                        val validTotalNFTAmount: Boolean = (nftPoolStateBoxOUT.R7[Long].get == SELF.R7[Long].get)
                        
                        val validRemainingNFTAmount: Boolean = (nftPoolStateBoxOUT.R8[Long].get == SELF.R8[Long].get)

                        allOf(Coll(
                            validMintAddress,
                            validNFTRaritiesAndAmounts,
                            validTokenRarityPackSizes,
                            validTotalNFTAmount,
                            validRemainingNFTAmount
                        ))

                    }

                    allOf(Coll(
                        validBox,
                        validValue,
                        validContract,
                        validTokens,
                        validRegisters
                    ))

                }

                validNFTPoolSateBoxReplication
                
            }

            sigmaProp(validWhitelistDistributionTx)

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
        val txOperatorBox: Box = OUTPUTS(OUTPUTS.size-2)                
        val minerBox: Box = OUTPUTS(OUTPUTS.size-1)

        val validNFTSaleTx: Boolean = {

            val validNFTPoolStateBoxReplication: Boolean = {

                val validValue: Boolean = {
                    (nftPoolStateBoxOUT.value == SELF.value) // must be 2 * MinERGForExistance in case of token retrieval instead of token burn for sale end tx
                }

                val validContract: Boolean = {
                    allOf(Coll(
                        (nftPoolStateBoxOUT.propositionBytes == SELF.propositionBytes)
                    ))
                }

                val validTokens: Boolean = {
                    
                    val validNFTPoolToken: Boolean = (nftPoolStateBoxOUT.tokens(0) == SELF.tokens(0))

                    val validWhitelistToken: Boolean = {

                        if (IsWhitelist) { 
                            (nftPoolStateBoxOUT.tokens(1) == SELF.tokens(1))
                        } else true
                    
                    }

                    allOf(Coll(
                        validNFTPoolToken,
                        validWhitelistToken
                    ))

                }

                val validRegisters: Boolean = {

                    val validMintAddress: Boolean = (nftPoolSateBoxOUT.R4[Coll[Byte]].get == SELF.R4[Coll[Byte]].get)

                    val validNFTRaritiesAndAmounts: Boolean = (nftPoolStateBoxOUT.R5[Coll[(Byte, Int)]].get == SELF.R5[Coll[(Byte, Int)]].get)

                    val validTokenRarityPackSizes: Boolean = (nftPoolStateBoxOUT.R6[Coll[Byte]].get == SELF.R6[Coll[Byte]].get)
                    
                    val validTotalNFTAmount: Boolean = (nftPoolStateBoxOUT.R7[Long].get == SELF.R7[Long].get)

                    val validRemainingNFTAmount: Boolean = {
                        allOf(Coll(
                            (remainingNFTAmount >= 0),
                            (nftPoolStateBoxOUT.R8[Long].get == SELF.R8[Long].get - 1)
                        ))
                    }

                }

                allOf(Coll(
                    validBox,
                    validValue,
                    validContract,
                    validTokens,
                    validRegisters
                ))

            }

            val validNFTPoolBoxReplication: Boolean = {

                val boxIndices: Coll[Int] = if (nftPoolBoxesIN.size == nftPoolBoxesOUT.size) nftPoolBoxesIN.indices else nftPoolBoxesIN.indices.slice(0, nftPoolBoxesIN.indices.size-1)
                val prefix: Coll[Byte] = Coll(0.toByte, 0.toByte, 0.toByte, 0.toByte)

                val validInputs: Boolean = {

                    val nftPoolBoxesINANDInputIndices: Coll[(Box, Int)] = nftPoolBoxesIN.zip(boxIndices)
                    
                    nftPoolBoxesINANDInputIndices.forall({ nftPoolBoxINANDBoxIndex: (Box, Int) =>

                        val nftPoolBoxIN: Box = nftPoolBoxINANDBoxIndex._1
                        val boxIndex: Int = nftPoolBoxINANDBoxIndex._2
                
                        val validValue: Boolean = (nftPoolBoxIN.value == MinERGForExistance)
                        
                        val validContract: Boolean = (blake2b256(nftPoolBoxIN.propositionBytes) == NFTPoolBoxContractHash)

                        val validNFTPoolToken: Boolean = (nftPoolBoxIN.tokens(0) == (NFTPoolToken, 1L))

                        // Check that the input indices of the pool boxes remain constant from each NFT sale tx to the next
                        val validInputIndex: Boolean = {

                            val nftPoolBoxINIndexBytes: Coll[Byte] = nftPoolBoxIN.creationInfo._2.slice(32, 36)  // Only the last 8 bytes are needed.
                            val nftPoolBoxINIndex: Int = byteArrayToLong(prefix.append(nftPoolBoxINIndexBytes)).toInt

                            (nftPoolBoxINIndex == boxIndex)

                        }

                        val validMintAddress: Boolean = (nftPoolBoxIN.R4[Coll[Byte]].get == MintAddress)

                        allOf(Coll(
                            validValue,
                            validContract,
                            validNFTPoolToken,
                            validInputIndex,
                            validMintAddress
                        ))

                    
                    })

                }

                val validOutputs: Boolean = {

                    val nftPoolBoxesOUTANDBoxIndices: Coll[(Box, Int)] = nftPoolBoxesOUT.zip(boxIndices)

                    nftPoolBoxesOUTANDBoxIndicies.forall({ nftPoolBoxOUTANDBoxIndex: (Box, Int) => 
                
                        val nftPoolBoxesOUT: Box = nftPoolBoxOUTANDBoxIndex._1
                        val boxIndex: Int = nftPoolBoxOUTANDBoxIndex._2

                        val validValue: Boolean = (nftPoolBoxOUT.value == MinERGForExistance)

                        val validContract: Boolean = (blake2b256(nftPoolBoxOUT.propositionBytes) == NFTPoolBoxContractHash)

                        val validNFTPoolToken: Boolean = (nftPoolBoxOUT.tokens(0) == (NFTPoolToken, 1L))

                        // Check that the output indices of the pool boxes remain constant from each NFT sale tx to the next
                        val validOutputIndex: Boolean = {
                            
                            val nftPoolBoxOUTIndexBytes: Coll[Byte] = nftPoolBoxOUT.creationInfo._2.slice(32, 36)  // Only the last 8 bytes are needed.
                            val nftPoolBoxOUTIndex: Int = byteArrayToLong(prefix.append(nftPoolBoxOUTIndexBytes)).toInt

                            (nftPoolBoxOUTIndex == boxIndex) 

                        }

                        val validMintAddress: Boolean = (nftPoolBoxIN.R4[Coll[Byte]].get == MintAddress)

                        allOf(Coll(
                            validValue,
                            validContract,
                            validNFTPoolToken,
                            validOutputIndex,
                            validMintAddress
                        ))

                    })

                }
            }

            allOf(Coll(
                validNFTPoolStateBoxReplication
                validNFTPoolBoxReplication,
                validMintAddress
            ))

        }

        sigmaProp(validNFTSaleTx)

    } else if (isNFTSaleEndTx) {

        // ===== Inputs ===== //
        val nftPoolStateBoxIN: Box = INPUTS(0)
        val nftPoolBoxesIN: Coll[Box] = INPUTS.slice(1, INPUTS.size)

        // ===== Outputs ===== //
        val userPKBoxesOUT: Coll[Box] = if (!IsTokenBurn) OUTPUTS.slice(0, OUTPUTS.size-2) else OUTPUTS
        val txOperatorBox: Box = OUTPUTS(OUTPUTS.size-2)
        val minerBox: Box = OUTPUTS(OUTPUTS.size-1)

        val validNFTSaleEndTx: Boolean = {

            val validNFTPoolStateBoxIN: Boolean = (nftPoolStateBoxIN == SELF)

            val validNFTPoolBoxesIN: Boolean = {
                
                nftPoolBoxesIN.forall({ nftPoolBoxIN: Box => 
                    
                    val validContract: Boolean = (blake2b256(nftPoolBoxesIN.propositionBytes) == NFTPoolBoxContractHash)
                    
                    val validNFTPoolToken: Boolean = (nftPoolBoxIN.tokens(0) == SELF.tokens(0))

                    allOf(Coll(
                        validContract,
                        validNFTPoolToken
                    ))
                
                })
            
            }

            val validUserPKBoxesOUT: Boolean = {
            
                if (!IsTokenBurn) {

                    userPKBoxesOUT.forall({ userPKBoxOUT: Box =>
                    
                        val validValue: Boolean = (userPKBoxOUT.value == MinERGForExistance)
                        
                        val validPK: Boolean = (userPKBoxOUT.propositionBytes == MintAddres)

                        val validTokens: Boolean = {

                            val nftCollection: Coll[(Coll[Byte], Long)] = INPUTS.fold(Coll[(Coll[Byte], Long)](), { (acc: Coll[(Coll[Byte], Long)], nftPoolBoxIN: Box) => acc ++ nftPoolBoxIN.tokens.slice(1, nftPoolBoxIN.tokens.size) })

                            (userPKBoxOUT.tokens == nftCollection)

                        }

                        allOf(Coll(
                            validValue,
                            validPK,
                            validTokens
                        ))
                    
                    })
                
                } else {
                    true
                }
                
            }

            val validTxOperatorBox: Boolean = {

                val validValue: Boolean = {

                    val inputValue: Long = INPUTS.fold(0L, { (acc: Long, input: Box) => input.value + acc })

                    if (IsTokenBurn) {

                        (txOperatorBox.value == inputValue - MinerFee)

                    } else {

                        (txOperatorBox.value == inputValue - MinERGForExistance - MinerFee)

                    }

                }

                val validPK: Boolean = (txOperatorBox.propositionBytes == TxOperatorPK)

                val validTokens: Boolean = {
                    
                    if (IsTokenBurn) {

                         // All input tokens must be burned
                        (txOperatorBox.tokens == Coll[(Coll[Byte], Long)]())
                    
                    } else {
                        true
                    }
                
                }

                allOf(Coll(
                    validValue,
                    validPK,
                    validTokens
                ))

            }

            val validMinerBox: Boolean = {
                
                val validValue: Boolean = {
                    (minerBox.value == MinerFee)
                }

                // All input tokens must be burned
                val validTokens: Boolean = {
                    (minerBox.tokens == Coll[(Coll[Byte], Long)]())
                }

                allOf(Coll(
                    validValue,
                    validTokens
                ))

            }

            allOf(Coll(
                validTimeExpired,
                validNFTPoolSateBoxIN,
                validNFTPoolBoxesIN,
                validUserPKBoxesOUT,
                validTxOperatorBox,
                validMinerBox
            ))

        }

        sigmaProp(validNFTSaleEndTx)

    } else {
        sigmaProp(false)
    }

}