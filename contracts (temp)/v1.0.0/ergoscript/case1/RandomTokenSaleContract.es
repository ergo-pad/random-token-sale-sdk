{
    
    // ===== Contract Description ===== //
    // Name: Random Token Sale Contract
    // Description: Contract which selects the tokens to sell to the buyer randomly.
    // Version: 1.0.0
    // Author: Luca D'Angelo

    // ===== Random Token Sale Tx ===== //

    // ===== Hard-Coded Constants ===== //
    val BuyerPK: SigmaProp = _BuyerPK
    val TokenSaleNFT: Coll[Byte] = _TokenSaleNFT
    val WhitelistToken: Option[Coll[Byte]] = _WhitelistToken
    val TokenPackSize: Byte = _TokenPackSize
    val NumberOfRarityPools: Byte = getVar[Byte](0).get
    val NumberOfIssuanceBoxes: Byte = getVar[Byte](1).get // for blitz this == TokenPackSize (i.e. token pack is just a subset of order |tokenPackSize| of the set of cards)

    val validRandomTokenSale: Boolean = {

        // ===== Inputs ===== //
        val rngOracleDataPointBoxIN: Box = DataInputs(0)
        val nftPoolStateBoxIN: Box = INPUTS(0)
        val nftRarityPoolBoxesIN: Coll[Box] = INPUTS.slice(1, NumebrOfRarityPools+1)
        val nftIssuanceBoxesIN: Box = INPUTS.slice(NumberOfRarityPools, NumberOfIssuanceBoxes+1)
        val nftSaleProxyBoxIN: Box = INPUTS(INPUTS.size-1)

        // ===== Outputs ===== //
        val nftPoolStateBoxOUT: Box = OUTPUTS(0)
        val nftRarityPoolBoxesOUT: Box = OUTPUTS.slice(1, NumberOfRarityPools+1) // fix this later
        val nftIssuanceBoxesOUT: Box = OUTPUTS.slice(NumberOfRarityPools, NumberOfIssuanceBoxes+1) // fix this later
        val buyerPKBoxOUT: Box = OUTPUTS(OUTPUTS.size-5)
        val userPKBoxOUT: Box = OUTPUTS(OUTPUTS.size-4)
        val ergopadBoxOUT: Box = OUTPUTS(OUTPUTS.size-3)
        val txOperatorBoxOUT: Box = OUTPUTS(OUTPUTS.size-2)
        val minerFeeBoxOUT: Box = OUTPUTS(OUTPUTS.size-1)

        val rngDataPoint: BigInt = rngBox.R6[BigInt].get
	    val rand: BigInt = (nftPoolStateBox.id * rng + SELF.id)

        val nftPoolSize: Short = nftPoolStateBox.R4[Short].get
        val nftRarityPoolsAmount: Short = nftPoolStateBox.R5[Short].get
        val nftRarityPoolsKeys: Coll[Short] = nftPoolStateBox.R6[Coll[Short]].get
        val nftPoolKeyIndex: Int = (rand % rarityPoolsAmount).toInt
        val nftPoolKey: Short = nftRarityKeys(nftPoolKeyIndex)
        val nftPoolProof: Coll[Byte] = getVar[Coll[Byte]](0).get
        val nftPoolAVLTree: AvlTree = nftPoolSateBox.R7[AvlTree].get
        val nftRarityPoolBoxId: Coll[Byte] = nftRarityPoolsAVLTree.get(longToByteArray(nftPoolKey.toLong), nftPoolProof)

        val nftRarityPoolSize: Short = nftRarityPoolBox.R4[Short].get
        val nftRarityPoolKeysANDAmounts: Coll[(Short, Long)] = nftRarityPoolBox.R5[Coll[(Short, Long)]].get
        val nftRarityPoolKeyIndex: Int = (rand % nftRarityPoolSize).toInt
        val nftRarityPoolToken: (Short, Long) = nftRarityPoolKeysANDAmounts(nftRarityPoolKeyIndex)
        val nftRarityPoolTokenKey: nftRarityPoolToken._1
        val nftRarityPoolTokenAmount: nftRarityPoolToken._2
        val nftRarityPoolTokenProof: Coll[Byte] = getVar[Coll[Byte]](1).get
        val nftRarityPoolAVLTree: AvlTree = nftRarityPoolBox.R6[AvlTree].get
        val nftIssuanceBoxId: Coll[Byte] = nftRarityPoolAVLTree.get(longToByteArray(nftRarityPoolTokenKey.toLong), nftRarityPoolProof)

        val validNFTRarityPoolSizeUpdate: Boolean = {

            if (nftRarityPoolTokenAmount == 1) {
                allOf(Coll(
                    (nftPoolStateBoxOUT.R4[Short].get == nftPoolSize - 1)
                    (nftRarityPoolBoxOUT.R4[Short].get == nftRarityPoolSize - 1),
                    (nftRarityPoolBoxOUT.R5[Coll[(Short, Long)]].get == nftRarityPoolKeysANDAmounts.filter({ token: (Short, Long) => token._1 != nftRarityPoolToken._1 }))
                ))   
            } else {
                allOf(Coll(
                    (nftRarityPoolBoxOUT.R4[Short].get == nftRarityPoolSize),
                    (nftRarityPoolBoxOUT.R5[Coll[(Short, Long)]].get.filter({ token: (Short, Long) => token._1 == nftRarityPoolToken._1})(0)._2 == nftRarityPoolTokenAmount - 1)
                ))
            }

        }

        val validNFTPoolSizeUpdate: Boolean = {

            if (nftRarityPoolSize == 1) {
                allOf(Coll(
                    (nftPoolStateBoxOUT.R5[Short].get == nftRarityPoolsAmount - 1),
                    (nftPoolStateBoxOUT.R6[Coll[Short]] == nftRarityKeys.filter({ rarityKey: Short => rarityKey != nftPoolKey }))
                ))
            } else {
                allOf(Coll(
                    (nftPoolStateBoxOUT.R5[Short].get == nftRarityPoolsAmount),
                    (nftPoolStateBoxOUT.R6[Coll[Short]] == nftRarityKeys)
                ))  
            }

        }

    }   
    
    sigmaProp(validNFTSale)

}

