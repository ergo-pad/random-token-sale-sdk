{

    // ===== Contract Description ===== //
    // Name: Token Collection Issuance Contract
    // Description: Issuance box contract for the token sale collection, following the EIP-34 collection standard.
    // Version: 1.0.0
    // Author: Luca D'Angelo

    // ===== Rarity Pool Modules Creation Tx ===== //
    // Description: This transaction creates the different rarity pool modules.
    // Data Inputs: None
    // Inputs: Token Collection Issuance Box 
    // Outputs: Token Sale Issuer, Token Pool Sate Box, Token Issuer i, Token Rarity Pool i

    // ===== Box Registers ===== //
    // Token: Whitelist Tokens
    // R4: Coll[Byte] => Collection Token Verbose Name
    // R5: Coll[Byte] => Collection Token Description
    // R6: Coll[Byte] => Collection Token Number of Decimals
    // R7: Coll[Byte] => Collection Token Asset Type

    // ===== Compile Time Constants ===== //
    // _RarityPoolsAmount: Long
    // _IsWhitelist: Boolean
    // _ProtocolBoxValue: Long
    // _TxOperatorPK: SigmaProp
    // _MinerFee: Long

    // ===== Context Extension Variables ===== //
    val TokenSaleIssuerContractBytes: Coll[Byte] = getVar[Coll[Byte]](0).get
    val TokenPoolStateBoxContractBytes: Coll[Byte] = getVar[Coll[Byte]](1).get
    val TokenIssuersContractBytes: Coll[Coll[Byte]] = getVar[Coll[Coll[Byte]]](2).get
    val TokenRarityPoolsContractBytes: Coll[Coll[Byte]] = getVar[Coll[Coll[Byte]]](3).get

    validRarityPoolModulesCreationTx: Boolean = {

        // ===== Outputs ===== //
        val tokenSaleIssuerBoxOUT: Box = OUTPUTS(0)
        val tokenPoolStateBoxOUT: Box = OUTPUTS(1)
        val tokenIssuerIndices: Coll[Int] = OUTPUTS.indices.filter({ (index: Int) => (index > 0) && (index % 2 == 0) }) // We only want the even indices greater than 0.
        
        val validTokenSaleIssuerBox: Boolean = {

            val validValue: Boolean = {
                (tokenSaleIssuerBoxOUT.value == _ProtocolBoxValue)
            }

            val validContract: Boolean = {
                (tokenSaleIssuerBoxOUT.propositionBytes == TokenSaleIssuerContractBytes)
            }

        }

        val validTokenPoolStateBox: Boolean = {

            val validValue: Boolean = {
                (tokenPoolStateBoxOUT.value == _ProtocolBoxValue)
            }

            val validContract: Boolean = {
                (tokenPoolStateBoxOUT.propositionBytes == TokenPoolStateBoxContractBytes)
            }

            val validWhitelistTokens: Boolean = {

                // The whitelist tokens must be transferred to the token pool state box, we do NOT include the collection tokens.
                if (_IsWhitelist) {

                    allOf(Coll(
                        SELF.tokens.slice(1, SELF.tokens.size).forall({ token: (Coll[Byte], Long) => token._2 == 1L }),
                        (tokenPoolStateBoxOUT.tokens == SELF.tokens.slice(1, SELF.tokens.size))
                    ))

                } else {
                    true
                }

            }

        }

        val validTokenRarityPoolModules: Boolean = {

            // We must check the following:
            // 1. the token issuer boxes (even boxes > 0)
            // 2. the token rarity pool boxes (odd boxes > 1)

            tokenIssuerIndices.forall({ (index: Int) =>

                val tokenIssuerBoxOUT: Box = OUTPUTS(index)
                val tokenRarityPoolBoxOUT: Box = OUTPUTS(index + 1)
            
                val validTokenIssuerBox: Boolean = {

                    val validValue: Boolean = {
                        (tokenIssuerBoxOUT.value = _ProtocolBoxValue)
                    }

                    val validContract: Boolean = {
                       (tokenIssuerBoxOUT.propositionBytes == TokenIssuersContractBytes(index - 2)) // Offset the first two output boxes
                    }

                    val validCollectionToken: Boolean = {
                        (tokenIssuerBoxOUT.tokens(0) == (SELF.tokens(0)._1, 1L)) // We transfer ONLY 1 collection token to the token issuer box
                    }

                }

                val validTokenRarityPoolBox: Boolean = {
                    
                    val validValue: Boolean = {
                        (tokenRarityPoolBoxOUT.value == _ProtocolBoxValue)
                    }

                    val validContract: Boolean = {
                        (tokenRarityPoolBoxOUT.propositionBytes == TokenRarityPoolsContractBytes(index - 2)) // Offset the first two output boxes
                    }

                }

                allOf(Coll(
                    validTokenIssuerBox,
                    validTokenRarityPoolBox
                ))
            
            })

        }

        val validMinerBox: Boolean = {
            (minerBoxOUT.value == _MinerFee)
        }

        allOf(Coll(
            validTokenSaleIssuerBox,
            validTokenPoolStateBox,
            validTokenRarityPoolModules,
            validMinerBox,
            (tokenIssuerIndices.size == _RarityPoolsAmount),            // Ensures all collection tokens are transferred and none are remaining.
            (OUTPUTS.size == (2 + (2 * tokenIssuerIndices.size) + 1))   // Ensures all boxes are accounted for, including the miner fee box, i.e. the + 1.
        ))

    }

    sigmaProp(validRarityPoolModulesCreationTx) && _TxOperatorPK

}