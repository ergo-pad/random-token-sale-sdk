{

    // ===== Contract Description ===== //
    // Name: Token Sale Setup Proxy
    // Description: Initiates the token minting process for the random token sale and holds any whitelist tokens for validation if a whitelist is needed.
    // Version: 1.0.0
    // Author: Luca D'Angelo

    // ===== Token Sale Setup Tx ===== //
    // Description: Transaction initiates the minting process and transfers any whitelist tokens used for validation.
    // Data Inputs: None
    // Inputs: Token Sale Setup Proxy
    // Outputs: Token Collection Issuer Box, ErgoPad Box, Tx Operator Box

    // ===== Box Registers ===== //
    // Tokens: Whitelist Tokens
    // R4: SigmaProp => UserPK

    // ===== Contract Compile Time Constants ===== //
    // _IsWhitelist: Boolean
    // _CollecionIssuerBoxValue: Boolean
    // _ErgoPadPK: SigmaProp
    // _TxOperatorPK: SigmaProp
    // _ErgoPadFee: Long
    // _TxOperatorFee: Long
    // _MinerFee: Long

    // ===== Contract Context Extension Variables ===== //
    val TokenCollectionIssuerContractBytes: Coll[Byte] = getVar[Coll[Byte]](0).get
    val CollectionStandardVersion: Int = getVar[Int](1).get
    val CollectionInfo: Coll[Coll[Byte]] = getVar[Coll[Coll[Byte]]](2).get
    val CollectionSocials: Coll[(Coll[Byte], Coll[Byte])] = getVar[Coll[(Coll[Byte], Coll[Byte])]](3).get
    val CollectionMintingExpiry: Long = getVar[Long](4).get // Should be -1 for now, no feature for send collections tokens back to the user is implemented yet, thus no timestamp should be possible.
    val CollectionAdditionalInfo: Coll[(Coll[Byte], Coll[Byte])] = getVar[Coll[(Coll[Byte], Coll[Byte])]](4).get

    val validTokenSaleSetupTx: Boolean = {

        // ===== Outputs ===== //
        val tokenCollectionIssuerBoxOUT: Box = OUTPUTS(0)
        val ergopadBoxOUT: Box = OUTPUTS(1)
        val txOperatorBoxOUT: Box = OUTPUTS(2)
        val minerBoxOUT: Box = OUTPUTS(3)

        val validTokenCollectionIssuerBox: Boolean = {

            val validValue: Boolean = {
                (tokenCollectionIssuerBoxOUT.value == _CollecionIssuerBoxValue)
            }

            val validContract: Boolean = {
                (tokenCollectionIssuerBoxOUT.propositionBytes == TokenCollectionIssuerContractBytes)
            }

            val validWhitelistTokens: Boolean = {

                // The whitelist tokens must be transferred to the token collection issuer box
                if (_IsWhitelist) {

                    allOf(Coll(
                        SELF.tokens.forall({ token: (Coll[Byte], Long) => token._2 == 1L }),
                        (tokenCollectionIssuerBoxOUT.tokens == SELF.tokens)
                    ))

                } else {
                    true
                }

            }

            val validCollectionIssuerProperties: Boolean = {

                allOf(Coll(
                    (tokenCollectionIssuerBoxOUT.R4[Int].get == CollectionStandardVersion),
                    (tokenCollectionIssuerBoxOUT.R5[Coll[Coll[Byte]]].get == CollectionInfo),
                    (tokenCollectionIssuerBoxOUT.R6[Coll[(Coll[Byte], Coll[Byte])]].get == CollectionSocials),
                    (tokenCollectionIssuerBoxOUT.R7[Long].get == CollectionMintingExpiry),
                    (tokenCollectionIssuerBoxOUT.R8[Coll[(Coll[Byte], Coll[Byte])]].get == CollectionAdditionalInfo)
                ))

            }

            allOf(Coll(
                validValue,
                validContract,
                validWhitelistTokens,
                validCollectionIssuerProperties
            ))

        }

        val validErgoPadBox: Boolean = {

            allOf(Coll(
                (ergopadBoxOUT.value == _ErgoPadFee),
                (ergopadBoxOUT.propositionBytes == ErgoPadPK.propBytes)
            ))

        }

        val validTxOperatorBox: Boolean = {

            allOf(Coll(
               ( txOperatorBoxOUT.value == _TxOperatorFee),
                (txOperatorBoxOUT.propositionBytes == _TxOperatorPK.propBytes)
            ))

        }

        val validMinerBox: Boolean = {
            (minerBoxOUT.value == _MinerFee)
        }

        allOf(Coll(
            validTokenCollectionIssuerBox,
            validErgoPadBox,
            validTxOperatorBox,
            validMinerBox,
            (OUTPUTS.size == 4)
        ))

    }

    sigmaProp(validTokenSaleSetupTx) && _TxOperatorPK

}