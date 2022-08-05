{
    
    // ===== Contract Description ===== //
    // Name: NFT Sale Proxy Contract

    // ===== Hard-Coded Constants ===== //
    val BuyerPK: SigmaProp = _BuyerPK
    val TokenPackSize: Byte = _TokenPackSize

    val validNFTSale: Boolean = {

        // ===== Inputs ===== //
        val rngBox: Box = DataInputs(0)
        val nftPoolStateBox: Box = INPUTS(0)
        val nftRarityPoolBox: Box = INPUTS(1)
        val nftIssuanceBox: Box = INPUTS(2)
        val nftSaleProxyBox: Box = INPUTS(3)

        // ===== Outputs ===== //
        val nftPoolStateBoxOUT: Box = OUTPUTS(0)
        val nftRarityPoolBoxOUT: Box = OUTPUTS(1)
        val nftIssuanceBoxOUT: Box = OUTPUTS(2)
        val buyerPKBoxOUT: Box = OUTPUTS(3)
        val userPKBox: Box = OUTPUTS(4)

        val rng: BigInt = rngBox.R6[BigInt].get
	    val rand: BigInt = (nftPoolStateBox.id * rng + SELF.id)

        val nftPoolSize: Short = nftPoolStateBox.R4[Short].get
        val nftRarityPoolsAmount: Short = nftPoolStateBox.R5[Short].get
        val nftRarityKeys: Coll[Short] = nftPoolStateBox.R6[Coll[Short]].get
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

