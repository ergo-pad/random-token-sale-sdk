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
    val NFTPoolToken: Coll[Byte] = _NFTPoolToken
    val WhitelistToken: Option[Coll[Byte]] = _WhitelistToken
    val BuyerPK: SigmaProp = _BuyerPK
    val TxOperatorPK: Coll[Byte] = _TxOperatorPK
    val NFTPrice: Long = _NFTPrice
    val MinERGForExistence: Long = 1000000L
    val TxOperatorFee: Long = _TxOperatorFee
    val MinerFee: Long = MinERGForExistance
    val NFTSaleCost: Long = MinERGForExistance + NFTPrice + TxOperatorFee + MinerFee
    val ErgoPadRNGOracleBoxContractHash: Coll[Byte] = blake2b256(_ErgoPadRNGORacleBoxContract)
    val NFTPoolStateBoxContractHash: Coll[Byte] = blake2b256(_NFTPoolStateBoxContract)
    val NFTPoolBoxContractHash: Coll[Byte] = blake2b256(_NFTPoolBoxContract)
    val BlockHeightLimit: Int = _BlockHeightLimit

    // ===== Spending Path Check ===== //
    val isNFTSaleTx: Boolean = (INPUTS.size >= 4)
    val isRefundTx: Boolean = (INPUTS.size == 1)

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
        val txOperatorBox: Box = OUTPUTS(OUTPUTS.size-2)                
        val minerBox: Box = OUTPUTS(OUTPUTS.size-1)

        // ===== Relevant Variables ===== //
        val randomness: BigInt = byteArrayToBigInt(ergopadRNGOracleDataInputBox.R5[Coll[Byte]].get)
        
        val nftCollection: Coll[(Coll[Byte], Long)] = nftPoolBoxesIN.fold(Coll[(Coll[Byte], Long)], { (acc: Coll[(Coll[Byte], Long)], nftPoolBox: Box) => acc ++ nftPoolBox.tokens.slice(1, nftPoolBox.tokens.size) })
        
        val mintAddress: Coll[Byte] = nftPoolStateBoxIN.R4[Coll[Byte]].get
        
        val nftRaritiesAndAmounts: Coll[(Byte, Int)] = nftPoolStateBoxIN.R5[Coll[(Byte, Int)]].get
        val rarities: Coll[Byte] = nftRaritiesAndAmounts.flatMap({ rarityAndAmount: (Byte, Int) => rarityAndAmount._1 })             // Defaults to 0 since all NFTs are of the same rarity/ordering/partition type
        val tokenRarityPoolSizes: Coll[Int] = nftRaritiesAndAmounts.flatMap({ rarityAndAmount: (Byte, Int) => rarityAndAmount._2 })  // The amount of NFTs belonging to a particular rarity/ordering/partition
        
        val tokenRarityPackSizes: Coll[Byte] = nftPoolStateBoxIN.R6[Coll[Byte]].get  // Defaults to 1 since only one NFT is given to the user
        val totalNFTAmount: Int = nftPoolStateBoxIN.R7[Int].get
        val remainingNFTAmount: Int = nftPoolStateBoxIN.R8[Int].get  // At the beginning of the sale, this value is equal to the totalNFTAmount

        val randomIndexForTokenPoolSelection: BigInt = draw(randomness, remainingNFTAmount)  // An index is generated using the random number from the oracle
        val nft: (Coll[Byte], Long) = nftCollection(randomIndexForTokenPoolSelection.toInt)

        // Check conditions for a valid NFT sale
        val validNFTSaleTx: Boolean = {

            val validTimeRemaining: Boolean = {

                val currentBlockHeight: Int = CONTEXT.preHeader.height

                (currentBlockHeight <= BlockHeightLimit)

            }

            val validRNGOracleBox: Boolean = {
                
                val validContract: Boolean = {
                    (blake2b256(ergopadRNGOracleBox.propositionBytes) == ErgoPadRNGOracleBoxContractHash)
                }

                val validRNGOracleNFT: Boolean = {

                    val rngOracleNFT: Coll[Byte] = ergopadRNGOracleBox.tokens(0).get._1
                    
                    (rngOracleNFT == ErgoPadRNGOracleNFT)
                
                }

                allOf(Coll(
                    validContract,
                    validRNGOracleNFT                                                        
                ))

            }

            val validNFTPoolStateBoxIN: Boolean = {
                allOf(Coll(
                    (blake2b256(nftPoolStateBoxIN.propositionBytes) == NFTPoolStateBoxContractHash),
                    (nftPoolStateBoxIN.tokens(0) == (NFTPoolToken, 1L))
                ))
            }

            val validNFTPoolBoxes: Boolean = {
                
                val validInputs: Boolean = {
                    
                    nftPoolBoxesIN.forall({ nftPoolBoxIN: Box => 
                    
                        val validContract: Boolean = (blake2b256(nftPoolBoxesIN.propositionBytes) == NFTPoolBoxContractHash)
                    
                        val validNFTPoolToken: Boolean = (nftPoolBoxIN.tokens(0) == NFTPoolToken)

                        allOf(Coll(
                            validContract,
                            validNFTPoolToken
                        ))
                
                    })
                }

                val validOutputs: Boolean = {

                    nftPoolBoxesOUT.forall({ nftPoolBoxOUT: Box => 
                    
                        val validContract: Boolean = (blake2b256(nftPoolBoxesOUT.propositionBytes) == NFTPoolBoxContractHash)
                    
                        val validNFTPoolToken: Boolean = (nftPoolBoxIN.tokens(0) == NFTPoolToken)

                        allOf(Coll(
                            validContract,
                            validNFTPoolToken
                        ))
                    
                    })

                }

                allOf(Coll(
                    validInputs,
                    validOutputs
                ))

            
            }

            val validNFTSaleProxyBox: Boolean = {
                
                val validNFTSaleCost: Boolean = {
                    (SELF.value == NFTSaleCost)
                }

                val validWhitelistToken: Boolean = {

                    if (WhitelistToken.isDefined) {
                        (SELF.tokens(0) == (WhitelistToken, 1L))
                    } else {
                        true
                    }
                    
                }

                // Check that the randomness used is generated after the user has submitted their funds to the proxy so they cannot determine a priori what NFT they will obtain
                val validRandomness: Boolean = {

                    val ergopadRNGOracleBoxHeight: Int = ergopadRNGOracleBox.creationInfo._1
                    val nftSaleProxyBoxInHeight: Int = SELF.creationInfo._1
                    
                    (nftSaleProxyBoxInHeight < ergopadRNGOracleBoxHeight)
                
                }

                allOf(Coll(
                    validNFTSaleCost,
                    validWhitelistToken,
                    validRandomness
                ))

            }

            val validBuyerPKBox: Boolean = {

                validValue: Boolean = {
                    (buyerPKBoxOUT.value == MinERGForExistance)
                }

                validPK: Boolean = {
                    (buyerPKBoxOUT.propositionBytes == BuyerPK.propBytes)
                }

                // Check that the user receives their NFT, their previous whitelist token, if required for the sale, is burned.
                validTokens: Boolean = {
                    (buyerPKBoxOUT.tokens == Coll(nft))
                }

                allOf(Coll(
                    validValue,
                    validPK,
                    validTokens
                ))

            }

            val validTxOperatorBox: Boolean = {

                // NOTE: When the amount of output NFT pool boxes is less than the amount of input NFT pool boxes, the ERG change will go to the tx operator PK.
                validValue: Boolean = {
                    (txOperatorBox.value == TxOperatorFee)
                }

                validPK: Boolean = {
                    (txOperatorBox.propositionBytes == TxOperatorPK)
                }

                allOf(Coll(
                    validValue,
                    validPK
                ))

            }

            val validMinerBox: Boolean = {

                validValue: Boolean = {
                    (minerBox.value == MinerFee)
                }

            }

            allOf(Coll(
                validTimeRemaining,
                validRNGOracleBox,
                validNFTPoolStateBox,
                validNFTPoolBoxes,
                validNFTSaleProxyBox,
                validBuyerPKBox,
                validTxOperatorBox, 
                validMinerBox
            ))

        }

        sigmaProp(validNFTSaleTx)


    } else if (isRefundTx) {

        // ===== Inputs ===== //
        val nftSaleProxyBoxIN: Box = INPUTS(0)

        // ===== Outputs ===== //
        val buyerPKBoxOUT: Box = OUTPUTS(0)
        val txOperatorBox: Box = OUTPUTS(1)
        val minerBox: Box = OUTPUTS(2)

        val validRefundTx: Boolean = {

            val validNFTSaleProxyBox: Boolean = {
                (SELF.value >= MinERGForExistance + TxOperatorFee + MinerFee)
            }

            val validBuyerPKBox: Boolean = {
                
                validValue: Boolean = {
                    (buyerPKBoxOUT.value == SELF.value - TxOperatorFee - MinerFee) 
                }

                validPK: Boolean = {
                    (buyerPKBoxOUT.propositionBytes == BuyerPK.propBytes)
                }

                validTokens: Boolean = {
                    (buyerPKBoxOUT.tokens == SELF.tokens)
                }

                allOf(Coll(
                    validValue,
                    validPK,
                    validTokens
                ))

            }

            val validTxOperatorBox: Boolean = {
                
                validValue: Boolean = {
                    (txOperatorBox.value == TxOperatorFee)
                }

                validPK: Boolean = {
                    (txOperatorBox.propositionBytes == TxOperatorPK)
                }

                allOf(Coll(
                    validValue,
                    validPK
                ))

            }

            val validMinerBox: Boolean = {

                validValue: Boolean = {
                    (minerBox.value == MinerFee)
                }

            }

            allOf(Coll(
                validNFTSaleProxyBox,
                validBuyerPKBox
                validTxOperatorBox,
                validMinerBox
            ))

        }

        sigmaProp(validRefundTx)

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