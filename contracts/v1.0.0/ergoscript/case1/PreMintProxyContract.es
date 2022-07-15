{

    // ===== Contract Information ===== //
    // Name: Pre-Mint Proxy Contract
    // Description: Proxy contract guarding the user's funds used for the minting of their NFT pool.
    // Version: 1.0.0
    // Author: Luca D'Angelo

    // ===== Pre-Mint Tx ===== //
    // Description: User sends funds to this proxy contract and the minting process begins.
    // DataInputs: None
    // Inputs: PreMintProxyBox
    // Context Extension Variables: None
    // Outputs: NFTIssuerProxyBoxes, TxOperatorBox

    // ===== Refund Tx ===== //
    // Description: User has sent funds to the pre-mint proxy but wants to cancel the minting process and retreive their funds back.
    // DataInputs: None
    // Inputs: PreMintProxyBox
    // Context Extension Variables: None
    // Outputs: UserPKBox, TxOperatorBox

    // ===== Hard-Coded Constants ===== //
    val MintAddress: SigmaProp = _MintAddress

    // ===== Spending Path Check ===== //
    val isPreMintTx: Boolean = !(OUTPUTS(0).propositionBytes == MintAddress.propBytes)
    val isRefundTx: Boolean = (OUTPUTS.size == 2)

    if (isPreMintTx) {

        val valid_PreMintTx: Boolean = {



        }

        sigmaProp(valid_PreMintTx)

    } else if (isRefundTx) {

        val valid_RefundTx: Boolean = {

            

        }
        
        sigmaProp(valid_RefundTx)

    } else {
        sigmaProp(false)
    }

}