
pragma solidity ^0.4.11;

contract Head {
    function HYDRA_INIT(address _sender);
}

contract SimpleMontyHall {

    /* ------- begin events ------- */
    event InFallback(bytes4 sig, address sender);
    event MultiCallReturn(bytes4 sig, uint256 ret, bool callSuccess);
    event HeadReturn(uint256 ret, bool callSuccess);
    event BountyPayed();

    /* ------- end events ------- */

    /* ------- begin private vars ------- */

    // Addresses of heads
    address[] private heads /*__JINJA_TEMPLATE__;= [{{heads}}]; /*__JINJA_TEMPLATE__*/;

    /* ------- end private vars ------- */


    /* ------- begin public vars ------- */

    // Whether the bounty has been claimed
    bool public bountyClaimed = false;

    // The amount of WEI to pay for a discovered discrepancy
    uint256 public bountyValue;

    // creator of this contract
    address public creator;

    /* ------- end public vars ------- */


    /* ------- begin constant vars ------- */

    // set to `true` to have the meta contract log events
    bool constant DEBUG_MODE = true;

    // used to count gas costs when DEBUG_MODE is on/off
    uint constant DEBUG_MODE_MUL = DEBUG_MODE ? 1 : 0;

    // maximum size iof a head's output
    uint256 constant MAX_HEAD_OUTPUT_SIZE = 6 * 0x20;

    /* ------- end constant vars ------- */


    /*
     * CONSTRUCTOR
     * Takes as argument the bounty value in WEI
     */
    function HydraContract() payable {
        bountyValue = msg.value;
        creator = msg.sender;
    }

    function HYDRA_INIT() {
        for (uint i=0; i< heads.length; i++) {
            Head(heads[i]).HYDRA_INIT(msg.sender);
        }
    }

    /* 
     * On an external call: dispatch the call to `multiCall`, specifying the
     *   message sender as the first argument.
     */
    function() payable {

        // get the signature of the called function
        bytes4 sig = msg.sig;

        // the contract cannot be called if the bounty has been claimed
        assert(!bountyClaimed);

        // the size of the output
        uint256 outputSize = 0x20;

        // the signature of the call to the heads
        bytes4 newSig;

        if (DEBUG_MODE) {
            InFallback(sig, msg.sender);
        }
        
        if ( sig == bytes4(sha3("InitMonty(int128,int128,bytes32)")) ) {
            newSig = bytes4(sha3("InitMonty(address,uint256,int128,int128,bytes32)"));
            outputSize = 0x0;

        } else if ( sig == bytes4(sha3("PlayMontyRound1(int128,int128)")) ) {
            newSig = bytes4(sha3("PlayMontyRound1(address,uint256,int128,int128)"));
            outputSize = 0x0;

        } else if ( sig == bytes4(sha3("OpenDoors(int128,int128)")) ) {
            newSig = bytes4(sha3("OpenDoors(address,uint256,int128,int128)"));

        } else if ( sig == bytes4(sha3("isOpened(int128,int128)")) ) {
            newSig = bytes4(sha3("isOpened(address,uint256,int128,int128)"));

        } else if ( sig == bytes4(sha3("PlayMontyRound2(int128,int128)")) ) {
            newSig = bytes4(sha3("PlayMontyRound2(address,uint256,int128,int128)"));

        } else if ( sig == bytes4(sha3("EndGame(int128,bytes32,int128)")) ) {
            newSig = bytes4(sha3("EndGame(address,uint256,int128,bytes32,int128)"));

        } else if ( sig == bytes4(sha3("Payout(bool,int128)")) ) {
            newSig = bytes4(sha3("Payout(address,uint256,bool,int128)"));

        } else if ( sig == bytes4(sha3("EscapeHatch()")) ) {
            newSig = bytes4(sha3("EscapeHatch(address,uint256)"));

        } else if ( sig == bytes4(sha3("RefundInactive(int128)")) ) {
            newSig = bytes4(sha3("RefundInactive(address,uint256,int128)"));

        } else if ( sig == bytes4(sha3("RefundAfterEscapeHatch(int128)")) ) {
            newSig = bytes4(sha3("RefundAfterEscapeHatch(address,uint256,int128)"));

        } else {
            revert();
        }
        
        // allocate memory to store the return value
        uint256 retValMem;
        uint256 maxOutputSize = MAX_HEAD_OUTPUT_SIZE;
        assembly {
            retValMem := mload(0x40)                    // free memory pointer
            mstore(0x40, add(retValMem, maxOutputSize)) // allocate output size
        }

        uint256 x;
        /* new call arguments are:
         *  newSig       -> x[0x0..0x3]
         *  msg.sender   -> x[0x4..0x23]
         *  msg.value    -> x[0x24..0x43]
         *  calldata[4:] -> x[0x44..]
         *
         * len(c) = 0x4 + 0x20 + 0x20 (calldatasize - 0x4)
         *        = calldatasize + 0x40
         */
        assembly {
            x := mload(0x40)                        // Get a free memory pointer

            mstore(x, newSig)                       // the new call signature
            mstore(add(x, 0x4), caller)             // the original sender
            mstore(add(x, 0x24), callvalue)         // the call value

            calldatacopy(add(x, 0x44), 0x4,         // Copy the call data       // calldatasize * 3 GAS
                         sub(calldatasize, 0x4))    // minus the signature

            mstore(0x40,                            // reset free memory pointer
                   add(x, add(0x40, calldatasize)))
        }

        // if multiCall returns `false`, a discrepancy has been found
        if (!multiCall(x, msg.data.length + 0x40, retValMem)) {
            payBounty();
            return;
        }

        // check if the call should throw
        bool callSuccess;
        assembly {
            callSuccess := mload(retValMem)
        }
        if (!callSuccess) {
            revert();
        }

        // skip the call success flag and return the output
        assembly {
            return(add(retValMem, 0x20), outputSize)
        }
    }

    function multiCall(uint256 args, uint256 argsLen, uint256 retValMem) internal returns (bool success) {

        // the hash of the output from the currently evaluated head.
        bytes32 retHash;

        // the hash of the output produced by the first head.
        bytes32 firstHeadRetHash;

        // the value returned by the current head
        uint256 retVal;

        // flag indicating whether the call to the head succeeded or not
        bool callSuccess;

        // max size of a head's output
        uint256 outputSize = MAX_HEAD_OUTPUT_SIZE;

        // execute all heads one after the other
        for (uint i = 0; i < heads.length; ++i) {                               // 200 GAS

            for (uint k = retValMem; k < retValMem + outputSize ; k+=0x20) {
                assembly {
                    mstore(k, 0)
                }
            }

            // call the head. skip the signature of `multiCall` in the args
            callSuccess = callHead(i,
                                   args,
                                   argsLen,
                                   retValMem);

            require(callSuccess);

            // get the hash of the return value
            // compute H(retMem[0:MAX_HEAD_OUTPUT_SIZE])
            assembly {
                retHash := sha3(retValMem, outputSize)                          // MAX_RETURN_BYTES/8 * 6
            }

            if (DEBUG_MODE) {

                assembly {
                    retVal := mload(retValMem)
                }

                HeadReturn(retVal, callSuccess);                                // 3 * 375 GAS
            }

            // first head
            if ( i == 0) {

                // save hash of output of first heads
                firstHeadRetHash = retHash;
            } else {

                if ( retHash != firstHeadRetHash ) {
                    // discrepancy between heads' outputs or throw behavior
                    return false;
                }
            }
        }

        /* the output is of the form
         * [CALL_SUCCESS,
         *  RET_VAL,
         *  ADDRESS1 (opt),
         *  VALUE1 (opt),
         *  ADDRESS2 (opt),
         *  VALUE2 (opt)]
         */
        address dest;
        uint256 val;
        assembly {
            dest := mload(add(retValMem, 0x40))
            val := mload(add(retValMem, 0x60))
        }

        if (val != 0x0) {
            if (!dest.send(val)) {
                revert();
            }
        }

        assembly {
            dest := mload(add(retValMem, 0x80))
            val := mload(add(retValMem, 0xa0))
        }

        if (val != 0x0) {
            if (!dest.send(val)) {
                revert();
            }
        }
        return true;
    }

    function callHead(uint256 i,
                      uint256 argsMem,
                      uint256 argsLen,
                      uint256 retValMem) private returns (bool callSuccess) {

        address dest = heads[i];                                                // 200 GAS

        uint256 headOutputSize = MAX_HEAD_OUTPUT_SIZE;

        assembly {
            callSuccess := call(                                                // 700 GAS
                gas,                    // give all gas
                dest,                   // destination address
                0,                      // value
                argsMem,                // inputs start here
                argsLen,                // input length
                retValMem,              // outputs will be written here
                headOutputSize          // size of outputs
            )
        }

        return callSuccess;
    }

    /*
     * Pays out the bounty when a discrepancy between heads is found
     */
    function payBounty() private {

        if (DEBUG_MODE) {
            BountyPayed();                                                      // 750 GAS
        }

        uint256 bal = this.balance - bountyValue;
        uint256 bounty = bountyValue + bal / 10;
        uint256 rest = this.balance - bounty;

        bountyClaimed = true;                                                   // 20000 GAS
        msg.sender.send(bounty);                                                //  9700 GAS
        creator.send(rest);                                                     //  9700 GAS
    }
}
