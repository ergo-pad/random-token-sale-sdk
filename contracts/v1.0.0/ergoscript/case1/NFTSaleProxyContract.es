{

    // ===== Contract Information ===== //
    // Name: NFT Sale Proxy Contract
    // Description: Proxy contract that selects only one NFT from the NFT pool.
    // Version: 1.0.0
    // Author: Luca D'Angelo

    // ===== NFT Sale Tx ===== //
    // Description: Sell one random NFT from the token pool to the user.
    // DataInputs: ErgoPadRNGOracleBox
    // Inputs: NFTPoolStateBox, NFTPoolBoxes, NFTSaleProxyBox
    // Context Extension Variables: None
    // Outputs: NFTPoolStateBox, NFTPoolBoxes, BuyerPKBox, TxOperatorBox

    // ===== Refund Tx ===== //
    // Description: Return funds from the NFTSaleProxyBox to the user's wallet.
    // DataInputs: None
    // Inputs: NFTSaleProxyBox
    // Context Extension Variables: None
    // Outputs: BuyerPK, TxOperatorBox

    // ===== Hard-Coded Constants ===== //
    val MintAddress: Coll[Byte] = _MintAddress
    val ErgoPadRNGOracleNFT: Coll[Byte] = _ErgoPadRNGOracleNFT
    val NFTPoolNFT: Coll[Byte] = _NFTPoolNFT
    val WhitelistNFT: Option[Coll[Byte]] = _WhitelistNFT
    val BuyerPK: SigmaProp = _BuyerPK
    val TxOperatorPK: SigmaProp = _TxOperatorPK
    val NFTPrice: Long = _NFTPrice
    val MinERGForExistence: Long = 1000000
    val TxOperatorFee: Long = _TxOperatorFee
    val MinerFee: Long = _MinerFee
    val NFTSaleCost: Long = MinERGForExistance + NFTPrice + TxOperatorFee + MinerFee
    val blockDelay: Byte = 5
    val ErgoPadRNGOracleBoxContractHash: Coll[Byte] = blake2b256(_ErgoPadRNGORacleBoxContract)
    val NFTPoolStateBoxContractHash: Coll[Byte] = blake2b256(_NFTPoolStateBoxContract)
    val NFTPoolBoxContractHash: Coll[Byte] = blake2b256(_NFTPoolBoxContract)

    // ===== Spending Path Check ===== //
    val isNFTSaleTx: Boolean = (INPUTS.size > 1)
    val isRefundTx: Boolean = (INPTUTS.size == 1)

    if (isNFTSaleTx) {

        // ===== Data Inputs ===== //
        val ergopadRNGOracleBox: Box = dataInputs(0)

        // ===== Inputs ===== //
        val nftPoolStateBoxIN: Box = INPUTS(0)
        val nftPoolBoxesIN: Coll[Box] = INPUTS.slice(1, INPUTS.size-1)
        val nftSaleProxyBoxIN: Box = INPUTS(INPUTS.size-1)

        // ===== Outputs ===== //
        val nftPoolStateBoxOUT: Box = OUTPUTS(0)
        val nftPoolBoxesOUT: Coll[Box] = OUTPUTS.slice(1, OUTPUTS,size-3)
        val buyerPKBoxOUT: Box = OUTPUTS(OUTPUTS.size-3)
        val txOperatorBoxOUT: Box = OUTPUTS(OUTPUTS.size-2)
        val minerBox: Box = OUTPUTS(OUTPUTS.size-1)

        // ===== Relevant Variables ===== //
        val randomness: BigInt = byteArrayToBigInt(ergopadRNGOracleDataInputBox.R5[Coll[Byte]].get)
        
        val nftCollection: Coll[(Coll[Byte], Long)] = nftPoolBoxesIN.flatMap({ (nftPoolBox: Box) => nftPoolBox.tokens.slice(1, nftPoolBox.tokens.size) })
        
        val mintAddress: Coll[Byte] = nftPoolStateBoxIN.R4[Coll[Byte]].get
        
        val nftRaritiesAndAmounts: Coll[(Byte, Int)] = nftPoolStateBoxIN.R5[Coll[(Byte, Int)]].get
        val rarities: Coll[Byte] = nftRaritiesAndAmounts.flatMap({ rarityAndAmount: (Byte, Int) => rarityAndAmount._1 })
        val tokenRarityPoolSizes: Coll[Int] = nftRaritiesAndAmounts.flatMap({ rarityAndAmount: (Byte, Int) => rarityAndAmount._2 })
        
        val tokenRarityPackSizes: Coll[Byte] = nftPoolStateBoxIN.R6[Coll[Byte]].get
        val totalNFTAmount: Int = nftPoolStateBoxIN.R7[Int].get
        val remainingNFTAmount: Int = nftPoolStateBoxIN.R8[Int].get

        val randomIndexForTokenPoolSelection: BigInt = draw(randomness, remainingNFTAmount)
        val nft: (Coll[Byte], Long) = nftCollection(randomIndexForTokenPoolSelection)

        val valid_NFTSaleTx: Boolean = {

            val valid_RNGOracleBox: Boolean = {
                
                val valid_Contract: Boolean = {
                    (blake2b256(ergopadRNGOracleBox.propositionBytes) == ErgoPadRNGOracleBoxContractHash)
                }

                val valid_RNGOracleNFT: Boolean = {
                    val rngOracleNFT: Coll[Byte] = ergopadRNGOracleBox.tokens(0).get._1
                    (rngOracleNFT == ErgoPadRNGOracleNFT)
                }

                val valid_Randomness: Boolean = {
                    val blockHeight: Int = CONTEXT.preheader.height
                    val rngOracleBoxHeight: Int = ergopadRNGOracleBox.creationInfo._1
                    (rngOracleBoxHeight < blockheight - BlockDelay)
                }

                allOf(Coll(
                    valid_RNGOracleNFT,
                    valid_Randomness
                ))

            }

            val valid_NFTPoolSateBoxReplication: Boolean = {\

                val valid_Value: Boolean = {
                    (nftPoolStateBoxIN.value == nftPoolStateBoxOUT)
                }

                val valid_Contract: Boolean = {
                    allOf(Coll(
                        (blake2b256(nftPoolStateBoxIN.propositionBytes) == NFTPoolStateBoxContractHash),
                        (nftPoolStateBoxOUT.propositionBytes == nftPoolStateBoxIN.propositionBytes)
                    ))
                }

                val valid_Tokens: Boolean = {
                    
                    val valid_NFTPoolNFT: Boolean = {
                        allOf(Coll(
                            (nftPoolStateBoxIN.tokens(0) == (NFTPoolNFT, 1L)),
                            (nftPoolStateBoxOUT.tokens(0) == nftPoolStateBoxIN.tokens(0))
                        ))
                    }

                    val valid_WhitelistNFT: Boolean = {
                        if (WhitelistNFT.isDefined) { 
                            allOf(Coll(
                                (nftPoolStateBoxIN.tokens(1) == (WhitelistNFT, 1L)),
                                (nftPoolSateBox_OUT.tokens(1) == nftPoolStateBoxIN.tokens(1))
                            ))
                        } else true
                    }

                    allOf(Coll(
                        valid_NFTPoolNFT,
                        valid_WhitelistNFT
                    ))

                }

                val valid_MintAddress: Boolean = {
                    allOf(Coll(
                        (mintAddress == MintAddress),
                        (nftPoolSateBox_OUT.R4[Coll[Byte]].get == mintAddress)
                    ))
                }

                val valid_RemainingNFTAmount: Boolean = {
                    allOf(Coll(
                        (remainingNFTAmount > 0),
                        (nftPoolStateBoxOUT.R8[Long].get == remainingNFTAmount - 1)
                    ))
                }

                allOf(Coll(
                    valid_Value,
                    valid_Contract,
                    valid_Tokens,
                    valid_MintAddress,
                    valid_RemainingNFTAmount
                ))

            }

            val valid_NFTPoolBoxReplication: Boolean = {

                val valid_Value: Boolean = {
                    allOf(Coll(
                        nftPoolBoxesIN.forall({ nftPoolBoxIN: Box => nftPoolBoxIN.value == MinERGForExistance }),
                        nftPoolBoxesOUT.forall({ nftPoolBoxOUT: Box => nftPoolBoxOUT.value == MinERGForExistance })
                    ))
                }

                val valid_Contract: Boolean = {
                    allOf(Coll(
                        nftPoolBoxesIN.forall({ nftPoolBoxIN: Box => blake2b256(nftPoolBoxIN.propositionBytes) == NFTPoolBoxContractHash }),
                        nftPoolBoxesOUT.forall({ nftPoolBoxOUT: Box => blake2b256(nftPoolBoxOUT.propositionBytes) == NFTPoolBoxContractHash })
                    ))
                }

                val valid_Tokens: Boolean = {

                    val valid_NFTPoolToken: Boolean = {
                        allOf(Coll(
                            nftPoolBoxesIN.forall({ nftPoolBoxIN: Box => nftPoolBoxIN.tokens(0) == (NFTPoolNFT, 1L) }),
                            nftPoolBoxesOUT.forall({ nftPoolBoxOUT: Box => nftPoolBoxOUT.tokens(0) == (NFTPoolNFT, 1L) })
                        ))
                    }

                    val valid_NFTRemoval: Boolean = {
                        (nftPoolBoxesOUT.tokens.size == nftPoolBoxIN.tokens.size - 1) 
                    }

                    allOf(Coll(
                        valid_NFTPoolToken,
                        valid_NFTRemoval
                    ))

                }

                val valid_InputIndex: Boolean = {
                    
                    val inputIndices: Coll[Int] = nftPoolBoxesIN.indices
                    val nftPoolBoxesINANDInputIndices: Coll[(Box, Int)] = nftPoolBoxesIN.zip(inputIndices)
                    val prefix: Coll[Byte] = Coll(0.toByte, 0.toByte, 0.toByte, 0.toByte)

                    nftPoolBoxesINANDInputIndices.forall({ nftPoolBoxINANDInputIndex: (Box, Int) => 
                        
                        val nftPoolBoxINIndexBytes: Coll[Byte] = nftPoolBoxINANDInputIndex._1.creationInfo._2.slice(32, 36)
                        val nftPoolBoxINIndex: Coll[Byte] = byteArrayToLong(prefix.append(nftPoolBoxINIndexBytes)).toInt

                        (nftPoolBoxINIndex == nftPoolBoxINANDInputIndex._2)

                    })    

                }
                
                val valid_OutputIndex: Boolean = {
                    
                    val boxIndices: Coll[Int] = if (nftPoolBoxesIN.size == nftPoolBoxesOUT.size) nftPoolBoxesIN.indices else nftPoolBoxesIN.indices.slice(0, nftPoolBoxesIN.indices.size-1)
                    val nftPoolBoxesOUTANDBoxIndices: Coll[(Box, Int)] = nftPoolBoxesOUT.zip(boxIndices)
                    val prefix: Coll[Byte] = Coll(0.toByte, 0.toByte, 0.toByte, 0.toByte)

                    nftPoolBoxesOUTANDInputIndices.forall({ nftPoolBoxOUTANDBoxIndex: (Box, Int) => 
                    
                        val nftPoolBoxOUTIndexBytes: Coll[Byte] = nftPoolBoxOUTANDBoxIndex._1.creationInfo._2.slice(32, 36)
                        val nftPoolBoxOUTIndex: Coll[Byte] = byteArrayToLong(prefix.append(nftPoolBoxOUTIndexBytes)).toInt

                        (nftPoolBoxOUTIndex == nftPoolBoxOUTANDBoxIndex._2)

                    })    
                    
                }

                val valid_MintAddress: Boolean = {
                    allOf(Coll(
                        nftPoolBoxesIN.forall({ nftPoolBoxIN: Box => nftPoolBoxIN.R4[Coll[Byte]].get == MintAddress })
                        nftPoolBoxesOUT.forall({ nftPoolBoxOUT: Box => nftPoolBoxOUT.R4[Coll[Byte]].get == MintAddress })
                    ))
                }

                allOf(Coll(
                    valid_Value,
                    valid_Contract,
                    valid_Tokens,
                    valid_InputIndex,
                    valid_OutputIndex,

                ))

            }

            val valid_NFTSaleProxyBox: Boolean = {
                
                val valid_NFTSaleCost: Boolean = {
                    (SELF.value == NFTSaleCost)
                }

                val valid_WhitelistNFT: Boolean = {
                    if (WhitelistNFT.isDefined) {
                        (SELF.tokens(0) == (WhitelistNFT, 1L))
                    } else {
                        true
                    }
                }

                allOf(Coll(
                    valid_NFTSaleCost,
                    valid_WhitelistNFT
                ))

            }

            val valid_BuyerPKBox: Boolean = {

                valid_Value: Boolean = {
                    (buyerPKBoxOUT.value == MinERGForExistance)
                }

                valid_PK: Boolean = {
                    (buyerPKBoxOUT.propositionBytes == BuyerPK.propBytes)
                }

                valid_Tokens: Boolean = {
                    (buyerPKBoxOUT.tokens == Coll(nft))
                }

                allOf(Coll(
                    valid_Value,
                    valid_PK,
                    valid_Tokens
                ))

            }

            val valid_TxOperatorBox: Boolean = {

                valid_Value: Boolean = {
                    (txOperatorBoxOUT.value == TxOperatorFee)
                }

                valid_PK: Boolean = {
                    (txOperatorBoxOUT.propositionBytes == TxOperatorPK.propBytes)
                }

                allOf(Coll(
                    valid_Value,
                    valid_PK
                ))

            }

            val valid_MinerBox: Boolean = {

                valid_Value: Boolean = {
                    (minerBox.value == MinerFee)
                }

            }

            allOf(Coll(
                valid_RNGOracleBox,
                valid_NFTPoolSateBoxReplication,
                valid_NFTPoolBoxReplication,
                valid_NFTSaleProxyBox,
                valid_BuyerPKBox,
                valid_TxOperatorBox, 
                valid_MinerBox
            ))

        }

        sigmaProp(valid_NFTSaleTx)


    } else if (isRefundTx) {

        // ===== Inputs ===== //
        val nftSaleProxyBoxIN: Box = INPUTS(0)

        // ===== Outputs ===== //
        val buyerPKBoxOUT: Box = OUTPUTS(0)
        val txOperatorBoxOUT: Box = OUTPUTS(1)
        val minerBox: Box = OUTPUTS(2)

        val valid_RefundTx: Boolean = {

            val valid_NFTSaleProxyBox: Boolean = {
                (SELF.value >= MinERGForExistance + TxOperatorFee + MinerFee)
            }

            val valid_BuyerPKBox: Boolean = {
                
                valid_Value: Boolean = {
                    (buyerPKBoxOUT.value == SELF.value - TxOperatorFee - MinerFee) 
                }

                valid_PK: Boolean = {
                    (buyerPKBoxOUT.propositionBytes == BuyerPK.propBytes)
                }

                valid_Tokens: Boolean = {
                    (buyerPKBoxOUT.tokens == SELF.tokens)
                }

                allOf(Coll(
                    valid_Value,
                    valid_PK,
                    valid_Tokens
                ))

            }

            val valid_TxOperatorBox: Boolean = {
                
                valid_Value: Boolean = {
                    (txOperatorBoxOUT.value == TxOperatorFee)
                }

                valid_PK: Boolean = {
                    (txOperatorBoxOUT.propositionBytes == TxOperatorPK.propBytes)
                }

                allOf(Coll(
                    valid_Value,
                    valid_PK
                ))

            }

            val valid_MinerBox: Boolean = {

                valid_Value: Boolean = {
                    (minerBox.value == MinerFee)
                }

            }

            allOf(Coll(
                valid_NFTSaleProxyBox,
                valid_BuyerPKBox
                valid_TxOperatorBox,
                valid_MinerBox
            ))

        }

        sigmaProp(valid_RefundTx)

    } else {
        sigmaProp(false)
    }

    // ===== Helper Methods ===== //

    // Method to generate ONE random number
    def draw(prev: BigInt, remainingNFTAmount: Int): BigInt = {

        val a: BigInt = byteArrayToBigInt(nftPoolStateBoxIN.id)
        val b: BigInt = byteArrayToBigInt(SELF.id)
        val next: BigInt = (a*seed + b) % remainingNFTAmount.toBigInt
        next
    
    }

}