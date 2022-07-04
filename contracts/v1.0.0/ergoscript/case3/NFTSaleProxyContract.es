{

    // ===== Contract Information ===== //
    // Name: NFT Sale Proxy Contract
    // Description: Proxy contract that selects the cards to give in a token pack, as long as the amount of different rarities in the pack is equal to the amount of different rarity pools.
    // Version: 1.0.0
    // Author: Luca D'Angelo

    // ===== Hard-Coded Constants ===== //
    val ErgoPadRNGOracleNFT: Coll[Byte] = _ErgoPadRNGOracleNFT
    val NFTPoolNFT: Coll[Byte] = _NFTPoolNFT
    val WhitelistNFT: Coll[Byte] = _WhitelistNFT
    val isWhitelist: Boolean = (WhitelistNFT != Coll(0.toByte))

    // ===== Relevant Inputs ===== //
    val ergopadRNGOracleDataInputBox: Box = dataInputs(0)
    val nftPoolStateBox: Box = INPUTS(0)
    val nftPoolBoxes: Coll[Box] = INPUTS.slice(1, INPUTS.size-1)
    val nftProxyBox: Box = INPUTS(INPUTS.size-1)
    
    // ===== Relevant Variables ===== //
    val randomness: BigInt = byteArrayToBigInt(ergopadRNGOracleDataInputBox.R5[Coll[Byte]].get)
    val nftCollection: Coll[(Coll[Byte], Long)] = nftPoolBoxes.flatMap({ (nftPoolBox: Box) => nftPoolBox.tokens.slice(1, tokens.size-1) })
    val nftIndices: Coll[Int] = nftCollection.indices
    val nftIndexCollection: Coll[Coll[Int]] = nftIndices.map(nftIndex: Int => Coll(nftIndex))

    val nftRaritiesAndAmounts: Coll[(Byte, Int)] = nftPoolStateBox.R5[Coll[(Byte, Int)]].get
    val rarities: Coll[Byte] = nftRaritiesAndAmounts.flatMap({ rarityAndAmount: (Byte, Int) => rarityAndAmount._1 })
    val tokenRarityPoolSizes: Coll[Int] = nftRaritiesAndAmounts.flatMap({ rarityAndAmount: (Byte, Int) => rarityAndAmount._2 })
    
    val tokenRarityPackSizes: Coll[Byte] = nftPoolStateBox.R6[Coll[Byte]].get    
    
    val tokenRarityIndexCollection: Coll[Coll[Coll[Int]]] = nftIndexCollection.slice(0, nftRaritiesAndAmounts.size).map(indexColl: Coll[Int] => Coll(indexColl))
    val tokenPoolTemp: Coll[Coll[(Coll[Byte], Long)]] = tokenRarityIndexCollection.fold(Coll(Coll((Coll(0.toByte), 0L))), { (acc: Coll[Coll[(Coll[Byte], Long)]], tokenRarityIndexColl: Coll[Coll[Int]]) => acc ++ Coll(nftCollection.slice(acc.size-1, tokenRarityPackSizes(acc.size-1))) })
    val tokenPool: Coll[Coll[(Coll[Byte], Long)]] = tokenPoolTemp.slice(1, tokenPoolTemp.size)

    val tokenRarityPackCollection: Coll[Coll[Coll[Byte]]] = tokenRarityPackSizes.map((tokenRarityPackSize: Byte) => Coll(Coll(tokenRarityPackSize)))    
    val randomIndicesForTokenRarityPackSelectionTemp: Coll[Coll[BigInt]] = tokenRarityPackCollection.fold(Coll(Coll(0.toBigInt)), { (acc: Coll[Coll[BigInt]], elem: Coll[Coll[Byte]]) => acc ++ Coll( gen( elem(0)(0), tokenRarityPoolSizes(acc.size-1), acc(acc.size-1)(acc(acc.size-1).size-1), randomness) ) })
    val randomIndicesForTokenRarityPackSelection:Coll[Coll[BigInt]] = randomNumbersForTokenRarityPackSelectionTemp.slice(1, randomNumbersForTokenRarityPackSelectionTemp.size)

    val tokenPoolANDRandomIndicesForTokenRarityPackSelection: Coll[(Coll[(Coll[Byte], Long)], Coll[BigInt])] = tokenPool.zip(randomIndicesForTokenRarityPackSelection)
    val tokenPack: Coll[Coll[(Coll[Byte], Long)]] = tokenPoolANDRandomIndicesForTokenRarityPackSelection.map( tokenPoolAndRandomIndices: (Coll[(Coll[Byte], Long)], Coll[BigInt]) => tokenPoolAndRandomIndices._2.map( randomIndex: BigInt => tokenPoolAndRandomIndices._1(randomIndex) ) )
 
    val validNFTSaleTx: Boolean = {



    }

    // ===== Helper Methods ===== //

    def draw(prev: BigInt, tokenRarityPoolSize: Int): BigInt = {

        val a: BigInt = byteArrayToBigInt(ergopadRNGOracleDataInputBox.id)
        val b: BigInt = byteArrayToBigInt(nftPoolStateBox.id)
        val next: BigInt = (a*prev + b) % tokenRarityPoolSize.toBigInt
        next

    }

    def gen(tokenRarityPackSize: Byte, tokenRarityPoolSize: Int, prev: BigInt, init: BigInt): Coll[BigInt] = {
        
        val seed: BigInt = if ( (prev == 0) || (prev == init) ) init else draw(prev, tokenRarityPoolSize)
        val iterableCollection: Coll[Coll[Int]] = nftIndexCollection.slice(0, tokenRarityPackSize)
        val randomNumbers: Coll[BigInt] = iterableCollection.fold(Coll(seed), { (acc: Coll[BigInt], elem: Coll[Int]) => acc ++ Coll(draw(acc(acc.size-1), tokenRarityPoolSize)) }) 
        randomNumbers.slie(0, randomNumbers.size-1)
    } 

}