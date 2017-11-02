

pragma solidity ^0.4.11;

contract HydraMintableERC20 {

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

    // Signature of the `multiCall` function
    bytes4 constant MULTI_CALL_SIG = bytes4(sha3("multiCall()"));

    // set to `true` to have the meta contract log events
    bool constant DEBUG_MODE = false;

    // used to count gas costs when DEBUG_MODE is on/off
    uint constant DEBUG_MODE_MUL = DEBUG_MODE ? 1 : 0;

    // rough estimate of gas used in the main loop of `multiCall`
    uint256 MULTI_CALL_LOOP_GAS = 200 + 200 + 700 + 200 + (DEBUG_MODE_MUL * 2 * 1125);

    /* ------- end constant vars ------- */


    /*
     * CONSTRUCTOR
     * Takes as argument the bounty value in WEI
     */
    function HydraContract() payable {
        bountyValue = msg.value;
        creator = msg.sender;
    }

    /* 
     * On an external call: dispatch the call to `multiCall`, specifying the
     *   message sender as the first argument.
     */
    function() {

        // get the signature of the called function
        bytes4 sig = msg.sig;

        // the contract cannot be called if the bounty has been claimed
        assert(!bountyClaimed);

        uint256 outputSize = 0x20;
        bytes4 newSig;

        if (DEBUG_MODE) {
            InFallback(sig, msg.sender);
        }

        if ( sig == bytes4(sha3("totalSupply()")) ) {
            newSig = bytes4(sha3("totalSupply(address)"));

        } else if ( sig == bytes4(sha3("balanceOf(address)")) ) {
            newSig = bytes4(sha3("balanceOf(address,address)"));

        } else if ( sig == bytes4(sha3("transfer(address,uint256)")) ) {
            newSig = bytes4(sha3("transfer(address,address,uint256)"));

        } else if ( sig == bytes4(sha3("transferFrom(address,address,uint256)")) ) {
            newSig = bytes4(sha3("transferFrom(address,address,address,uint256)"));

        } else if ( sig == bytes4(sha3("approve(address,uint256)")) ) {
            newSig = bytes4(sha3("approve(address,address,uint256)"));

        } else if ( sig == bytes4(sha3("allowance(address,address)")) ) {
            newSig = bytes4(sha3("allowance(address,address,address)"));

        } else if ( sig == bytes4(sha3("mint(address,uint256)")) ) {
            newSig = bytes4(sha3("mint(address,address,uint256)"));

        } else if ( sig == bytes4(sha3("finishMinting()")) ) {
            newSig = bytes4(sha3("finishMinting(address)"));

        } else {
            revert();
        }

        // allocate memory to store the return value
        uint256 retValMem;
        assembly {
            retValMem := mload(0x40)              // free memory pointer
            mstore(0x40,                          // allocate output size
                   add(retValMem, add(outputSize, 0x20)))
        }

        /*
         * Status of the call to `multiCall`. If `multiCall` fails, we pay
         * out the bounty.
         */
        bool callSuccess;

        /*
         * signature of `multiCall()`
         */
        bytes4 multiCallSig = MULTI_CALL_SIG;

        /*
         * Make sure that there will be enough gas for `multiCall` to work
         */
        uint256 totCost = totalGasCost(msg.data.length);
        assert( msg.gas >= totCost );
        
        // new call arguments are:
        // [multiCallSig(0x4) newSig(0x4) msg.sender(0x20) origArgs[0x4:]]
        // len(args) = 0x4 + 0x4 + 0x20 + (calldatasize - 0x4)
        //           = calldatasize + 0x24
        
        assembly {
            let x := mload(0x40)                    // Get a free memory pointer
            mstore(x, multiCallSig)                 // Copy sig of multiCall

            mstore(add(x, 0x4), newSig)             // the new call signature
            mstore(add(x, 0x8), caller)             // the original sender

            calldatacopy(add(x, 0x28), 0x4,         // Copy the call data       // calldatasize * 3 GAS
                         sub(calldatasize, 0x4))    // minus the signature

            mstore(0x40,                            // reset free memory pointer
                   add(x, add(0x24, calldatasize)))

            callSuccess := call(
                gas,                        // Keep gas to pay bounty           // 700 GAS
                address,                    // Destination address
                0x0,                        // No value sent
                x,                          // Inputs start here
                add(0x24, calldatasize),    // Total length of inputs
                retValMem,                  // Outputs written here
                add(outputSize, 0x20))      // Size of outputs
        }

        if ( !callSuccess ) {
            // If we're here, `multiCall` threw to indicate a discrepancy

            if (DEBUG_MODE) {
                MultiCallReturn(sig, 0x0, callSuccess);                         // 4 * 375 GAS
            }

            payBounty();                                                        // PAYBOUNTY_GAS
            return;
        }

        /*
         * Check whether this call should throw
         */
        uint256 throwStatus;

        // read the first word of the return data
        assembly {
            throwStatus := mload(retValMem)
        }

        if (DEBUG_MODE) {
            uint256 retVal;

            assembly {
                retVal := mload(add(retValMem, 0x20))
            }

            MultiCallReturn(sig, retVal, callSuccess);
        }

        /*
         * If the heads agree that the call should throw, `multiCall`
         * returns -1
         */
        if ( throwStatus == uint(~0) ) {
            revert();
        }

        assembly {
            return(add(retValMem, 0x20), outputSize)
        }
    }

    /*
     * Called by the default function to add a layer of indirection to the
     * calls to the heads. If this function notices a discrepancy between heads,
     * it throws, thus reverting any changes the discrepancy may have
     * introduced.
     * The default function can then "catch" this exception and pay out the
     * bounty.
     */
    function multiCall() external {

        // We cannot mark this function as internal as we need to call it
        // explicitly from the fallback function.
        assert(msg.sender == address(this));

        uint256 gasForHeads = totalGasForHeads(msg.data.length - 0x4);

        // the hash of the output from the currently evaluated head.
        bytes32 retHash;

        // the hash of the output produced by the first head.
        bytes32 firstHeadRetHash;

        // the maximum size of the head's output.
        uint256 outputSize = 0x20;

        // allocate memory to store the return value
        uint256 retValMem;
        assembly {
            retValMem := mload(0x40)            // free memory pointer
            mstore(0x40,                        // update memory pointer
                   add(retValMem,
                       add(outputSize, 0x20)))

        }
        uint256 retVal;

        bool callSuccess;

        // copy the call arguments into memory
        // layout: [sigOfMultiCall(4) argsForHead]
        uint256 argsMem;
        assembly {
            argsMem := mload(0x40)                 // free memory pointer
            calldatacopy(argsMem,                  // copy all the args         // 3 * calldatasize GAS
                         0x4,
                         sub(calldatasize, 0x4))
            mstore(0x40,                           // update the memory pointer
                   add(argsMem,
                       sub(calldatasize, 0x4)))
        }

        // keep track of the gas used by the heads
        uint256 gasPreCall;
        uint256 gasUsedByHead;

        // execute all heads one after the other
        for (uint i = 0; i < heads.length; ++i) {                               // 200 GAS

            gasPreCall = msg.gas;

            callSuccess = callHead(i,
                                   gasForHeads,
                                   argsMem,
                                   msg.data.length,
                                   retValMem,
                                   outputSize);

            // compute the amount of gas we spent on this call
            gasUsedByHead = gasPreCall - msg.gas;

            // if the CALL didn't succeed, the head throws
            if ( !callSuccess ) {
                assembly {
                    mstore(retValMem, not(0))                   // throwStatus
                    return(retValMem, add(outputSize, 0x20))    // return
                }
            } else {
                assembly {
                    mstore(retValMem, 0)
                }
            }

            // update the amount of gas available for the remaining heads
            gasForHeads -= gasUsedByHead;

            // get the hash of the return value
            // compute H(throwStatus, retMem[0:outputSize])
            assembly {
                retHash := sha3(retValMem, add(outputSize, 0x20))               // MAX_RETURN_BYTES/8 * 6
            }

            if (DEBUG_MODE) {

                assembly {
                    retVal := mload(add(retValMem, 0x20))
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
                    // wrapper will pay bounty
                    revert();
                }
            }
        }

        assembly {
            return(retValMem, add(outputSize, 0x20))                            // 3 * (MAX_RETURN_BYTES + 0x40)
        }
    }

    function callHead(uint256 i,
                      uint256 gasPerHead,
                      uint256 argsMem,
                      uint256 argsLen,
                      uint256 retValMem,
                      uint256 outputSize) private returns (bool callSuccess) {

        address dest = heads[i];                                                // 200 GAS

        assembly {
            callSuccess := call(                                                // 700 GAS
                gasPerHead,             // keep enough gas to avoid OOG
                dest,                   // destination address
                0,                      // value
                argsMem,                // inputs start here
                argsLen,                // input length
                add(retValMem, 0x20),   // outputs will be written here
                outputSize              // size of outputs
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

    /*
     * Estimate of gas cost in `multiCall`
     */
    function multiCallGasCost(uint256 callDataSize) private returns(uint256 totalGas) {
        uint256 numHeads = heads.length;

        uint256 preLoopCost = (callDataSize * 3);
        uint256 firstHeadCost = MULTI_CALL_LOOP_GAS + 200;
        uint256 otherHeadCost = MULTI_CALL_LOOP_GAS + 400;
        uint256 postLoopCost = 0;

        uint256 tot = preLoopCost + firstHeadCost + (numHeads - 1) * otherHeadCost + postLoopCost;
        return tot;
    }

    /*
     * Estimate of gas cost in default function
     */
    function totalGasCost(uint256 callDataSize) private returns(uint256 totalGas) {
        uint256 preCost = 700 + (callDataSize * 3);
        uint256 multiCallCost = multiCallGasCost(callDataSize);
        uint256 postCost = 0;

        uint256 tot = (preCost + multiCallCost + postCost);

        return tot + tot / 10;
    }

    function totalGasForHeads(uint256 callDataSize) public returns(uint256 gasAmount) {
        uint256 minGasCost = multiCallGasCost(callDataSize);
        uint256 totalGas = msg.gas;
        uint256 gasForHeads = (totalGas - minGasCost - minGasCost / 10);

        return gasForHeads;
    }
}
