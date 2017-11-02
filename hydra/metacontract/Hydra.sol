/*
 * Call-Logic: 
 *   1) The fallback function dispataches the call to the multiCall function
 *   2) The multiCall function calls each head sequentially
 *   3) The multiCall returns the response (or signals a discrepancy)
 *   4) The fallback function returns to the caller or pays out the bounty
 *
 *  |      Head1     |      Head2     |   multiCall   |  fallback  |
 *  =================================================================
 *  | return a       | return a       | return a       | return a   |
 *  | return a       | return b       | throw          | pay bounty |
 *  | return "throw" | return a       | throw          | pay bounty |
 *  | return "throw" | return "throw" | return "throw" | throw      |
 *  | throw (OOG)    | N.A.           | return "throw" | throw      |
 *  =================================================================
 */

pragma solidity ^0.4.11;

contract HydraContract {

    /* ------- begin events ------- */
    event MultiCallReturn(bytes4 sig, uint256 retSize, bool callSuccess);
    event HeadReturn(uint256 retSize, bool callSuccess);

    event InCallBack(uint256 calldatasize, uint256 callType, address sender);
    event BalanceCallBack(address addr, uint256 bal);
    event LogCallBack(bytes32 hash, uint256 numTopics, uint256 memLen);
    event ExternalCallBack(bytes4 sig, uint256 gasVal, uint256 weiVal, address destAddr, uint256 outputSize, uint256 argsLen);
    event RepeatCallback(uint256 callType, uint256 outputSize, bytes32 prevHash, bytes32 currHash);
    event CallBackDone(uint256 callType, address sender);

    event BountyPayed();
    /* ------- end events ------- */

    /* ------- begin private vars ------- */

    // if heads have no constructor, consider them initialized at compile time
    bool private headsInitialized /*__JINJA_TEMPLATE__;= {{ 'false' if init_sig != None else 'true' }}; /*__JINJA_TEMPLATE__*/;

    // Addresses of heads are set at instrumentation time before compilation
    address[] private heads /*__JINJA_TEMPLATE__;= [{{heads}}]; /*__JINJA_TEMPLATE__*/;

    // Storage value set if a discrepancy is noted between heads during
    // a callback
    bool private discrepancy;

    // Structure to save callbacks made during execution of the first head
    struct callData {
        bytes32 hash;              // SHA3 of the callback data
        bytes returnValue;         // the return value (dynamically allocated)
    }

    // Saves all the callbacks made by the first head, so that they can be
    // replayed for further heads
    mapping(uint256 => callData) private calls;

    // Pointer to the `callData` structure in `calls` to use for the head's
    // next callback
    uint256 private nextCall;

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

    // Maximum number of bytes for the return value of a head
    // Does not include the first word that specifies the actual output size
    uint256 constant MAX_RETURN_BYTES =  0x100;

    // Signature of the `multiCall` function
    bytes4 constant MULTI_CALL_SIG = bytes4(sha3("multiCall()"));

    // Values used to differentiate between the type of head callbacks
    uint256 CALLBACK_TYPE_LOG = 4;
    uint256 CALLBACK_TYPE_CALL = 5;
    uint256 CALLBACK_TYPE_BALANCE = 6;

    // set to `true` to have the meta contract log events
    bool constant DEBUG_MODE /*__JINJA_TEMPLATE__;= {{ 'true' if debug else 'false' }}; /*__JINJA_TEMPLATE__*/ = false;

    // used to count gas costs when DEBUG_MODE is on/off
    uint constant DEBUG_MODE_MUL = DEBUG_MODE ? 1 : 0;

    // rough estimate of gas used in the main loop of `multiCall`
    uint256 MULTI_CALL_LOOP_GAS = 200 + 200 + 700 + 200 + (DEBUG_MODE_MUL * 2 * 1125) + (MAX_RETURN_BYTES/8 * 6);

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
     * Most of the logic in this contract happens in the default function:
     *
     * - On an external call: dispatch the call to `multiCall`, specifying the
     *   message sender and value as the first two arguments.
     *   External calls do not allow reentrancy.
     *
     * - On a callback from one of the heads, record and handle the callback.
     */
    function() external payable {

        bool isHydraInitCall = false;

        // get the signature of the called function
        bytes4 sig = msg.sig;

        /*
         * check whether it is a call to the special `HYDRA_INIT` function used
         * to initialize the heads
         */
        /*__JINJA_TEMPLATE__
        IMPORTANT: THE TEMPLATE BELOW SHOULD BE EVALUATED AND THE PLACEHOLDER
        REPLACED BY THE SIGNATURE OF THE HYDRA_INIT FUNCTION;
        {% if init_sig != None %}
        if ( sig == {{init_sig}} ) {
            isHydraInitCall = true;
        }

        // either this is a call to HYDRA_INIT or the heads are initialized
        assert(isHydraInitCall != headsInitialized);

        // mark heads as initialized
        headsInitialized = true;

        {% endif %}
        //{{success_msg}}
        /*__JINJA_TEMPLATE__*/revert();

        // the contract cannot be called if the bounty has been claimed
        assert(!bountyClaimed);

        // check if this is a callback from one of the heads
        for (uint i = 0; i < heads.length; i++) {
            if (msg.sender == heads[i]) {
                headCallback();
                return;
            }
        }

        /*
         * Check that the caller is calling a function that is defined in all
         * the heads.
         */
        /*__JINJA_TEMPLATE__
        IMPORTANT: THE TEMPLATE BELOW SHOULD BE EVALUATED, AND PLACEHOLDERS
        REPLACED BY THE SIGNATURES OF ALLOWED FUNCTION CALLS;
        // Beginning of Jinja template
        if ( sig == 0x0 ) {}
        {% for sig in sigs %}
        else if ( sig == {{sig}} ) {}
        {% if loop.last %}
        else { revert(); }
        {% endif %}
        {% endfor %}
        //{{success_msg}}
        /*__JINJA_TEMPLATE__*/revert();

        // non-reentrancy lock
        assert(nextCall == 0);

        /*
         * Maximum byte size of the return value from a head. The structure of
         * the returned data is [retSize retVal numCalls] where retSize is
         * a word specifying the actual length of the return value, retVal is
         * a sequence of length `MAX_RETURN_BYTES` containing the return value
         * in the first `retSize` bytes and numCalls is a word specifiyng the
         * number of callbacks that were made.
         */
        uint256 outputSize = 0x20 + MAX_RETURN_BYTES + 0x20;

        // allocate memory to store the return value
        uint256 retValMem;
        assembly {
            retValMem := mload(0x40)              // free memory pointer
            mstore(0x40,                          // allocate max output size
                   add(retValMem, outputSize))
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

        /*
         * The call to `multiCall` will have the following arguments:
         *
         * [multiCallSig(0x4) msg.sender(0x20) msg.value(0x20) calldata(calldatasize)]
         *
         * The return value will be of the form:
         *
         * [retSize(0x20) output(retSize) numCallbacks(0x20)]
         */
        assembly {
            let x := mload(0x40)                // Get a free memory pointer
            mstore(x, multiCallSig)             // Copy signature of multiCall

            mstore(add(x, 0x4), caller)         // the original message sender
            mstore(add(x, 0x24), callvalue)     // the original message value

            calldatacopy(add(x, 0x44),
                         0x0, calldatasize)     // Copy this call's calldata    // calldatasize * 3 GAS

            mstore(0x40,                        // reset free memory pointer
                   add(x, add(0x44, calldatasize)))

            callSuccess := call(
                gas,                        // Give all gas                     // 700 GAS
                address,                    // Destination address
                0x0,                        // Value sent in extra arg
                x,                          // Inputs start here
                add(0x44, calldatasize),    // Total length of inputs
                retValMem,                  // Outputs written here
                outputSize)                 // Size of outputs
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
         * The actual size of the return value
         */
        uint256 retSize;

        // read the return size
        assembly {
            retSize := mload(retValMem)
        }

        if (DEBUG_MODE) {
            MultiCallReturn(sig, retSize, callSuccess);
        }

        /*
         * If the heads agree that the call should throw, `multiCall`
         * returns -1
         */
        if ( retSize == uint(~0) ) {
            revert();
        }

        /*
         * Reset all temporary storage used to get a gas reimbursement.
         * No need to reset `discrepancy` as `multiCall` throws when it is set
         */
        uint256 numCallbacks;

        // read the number of callbacks made
        assembly {
            numCallbacks := mload(add(retValMem, sub(outputSize, 0x20)))
        }

        for (i=0; i<numCallbacks; i++) {
            delete calls[i].hash;
            if (calls[i].returnValue.length > 0) {
                delete calls[i].returnValue;
            }
        }

        if (numCallbacks != 0) {
            nextCall = 0;
        }

        // return the actual output
        assembly {
            return(add(retValMem, 0x20), retSize)
        }
    }

    /*
     * Called by the default function to add a layer of indirection to the
     * calls to the heads. If this function notices a discrepancy between heads,
     * it throws, thus reverting any changes the discrepancy may have
     * introduced.
     * The default function can the "catch" this exception and pay out the
     * bounty.
     */
    function multiCall() external {

        // We cannot mark this function as internal as we need to call it
        // explicitly from the fallback function.
        assert(msg.sender == address(this));

        // compute the amount of gas we can give the heads
        uint256 gasForHeads = totalGasForHeads(msg.data.length - 0x4);

        // the hash of the output from the currently evaluated head.
        bytes32 retHash;

        // the hash of the output produced by the first head.
        bytes32 firstHeadRetHash;

        // the maximal allowed size of the head's output. The structure of the
        // returned value is [retSize retVal] where retSize is a word
        // specifying the actual size in bytes of the return value and retVal
        // is a sequence of retSize bytes (with retSize < MAX_RETURN_BYTES).
        // If the head wants to throw, it sets retSize = -1
        uint256 outputSize = 0x20 + MAX_RETURN_BYTES;

        // allocate memory to store the return value
        uint256 retValMem;
        assembly {
            retValMem := mload(0x40)            // free memory pointer
            mstore(0x40,
                   add(retValMem,               // add an extra word to store
                       add(outputSize, 0x20)))  // the number of callbacks

        }

        // the success flag for the call to the head
        bool callSuccess;

        // the size of the head's output
        uint256 retSize;

        /*
         * Copy the call arguments into memory
         * The arguments received from the fallback function are:
         * [multiCallSig(0x4) restOfArgs(calldatasize - 0x4)]
         */
        uint256 argsMem;
        assembly {
            argsMem := mload(0x40)                 // free memory pointer

            calldatacopy(argsMem,                  // copy the args             // 3 * (calldatasize - 0x4) GAS
                         0x4,
                         sub(calldatasize, 0x4))
            mstore(0x40,                            // update the memory pointer
                   add(argsMem,
                       sub(calldatasize, 0x4)))
        }


        // keep track of number of calls made by first head
        uint256 numCallsFirstHead = 0;

        // keep track of the gas used by the heads
        uint256 gasPreCall;
        uint256 gasUsedByHead;

        // execute all heads one after the other
        for (uint i = 0; i < heads.length; ++i) {                               // 200 GAS

            gasPreCall = msg.gas;

            // reset the storage value used to keep track of the next
            // `CallData` value to use to handle a callback
            if (nextCall != 0) {                                                // 200 GAS
                nextCall = 0;                                                   // 5000 GAS
            }

            callSuccess = callHead(i,
                                   gasForHeads,
                                   argsMem,
                                   msg.data.length - 0x4,
                                   retValMem,
                                   outputSize);

            // compute the amount of gas we spent on this call
            gasUsedByHead = gasPreCall - msg.gas;

            // if the CALL didn't succeed (probably OOG),
            // return -1 so the default function will throw
            if ( !callSuccess ) {

                if (DEBUG_MODE) {
                    HeadReturn(0, callSuccess);                                 // 3 * 375 GAS
                }

                assembly {
                    mstore(retValMem, not(0))
                    return(retValMem, 0x20)
                }
            }

            // update the amount of gas available for the remaining heads
            gasForHeads -= gasUsedByHead;

            // get the size of the return value in bytes
            assembly {
                retSize := mload(retValMem)
            }

            if ( retSize == uint(~0) ) {
                // head signals a throw by returning -1
                retHash = bytes32(retSize);
            } else if ( retSize > MAX_RETURN_BYTES) {
                // this shouldn't happen, bad instrumentation!
                revert();
            } else {
                // compute H(retVal, retMem[0:retVal])
                assembly {
                    retHash := sha3(add(retValMem, 0x20), retSize)              // MAX_RETURN_BYTES/8 * 6 GAS
                }
            }

            if (DEBUG_MODE) {
                HeadReturn(retSize, callSuccess);                               // 3 * 375 GAS
            }

            // first head
            if ( i == 0) {
                // save number of calls made by first head
                numCallsFirstHead = nextCall;                                   // 200 GAS

                // save hash of output of first heads
                firstHeadRetHash = retHash;
            } else {

                if ( retHash != firstHeadRetHash ) {
                    // discrepancy between heads' outputs or throw behavior
                    // wrapper will pay bounty
                    revert();
                }

                if ( retSize != uint(~0) &&
                    (discrepancy || nextCall != numCallsFirstHead) ) {          // 400 GAS
                    // some discrepancy in the type or number of callbacks
                    // wrapper will pay bounty
                    throw;
                }
            }
        }

        assembly {
            // store the number of callbacks made so we can erase the data
            mstore(add(retValMem, outputSize), numCallsFirstHead)

            // retValMem=[retSize(0x20) output(MAX_OUTPUT_BYTES) numCalls(0x20)]
            return(retValMem, add(outputSize, 0x20))                            // 3 * (MAX_RETURN_BYTES + 0x40)
        }
    }

    function callHead(uint256 i,
                      uint256 gasForHeads,
                      uint256 argsMem,
                      uint256 argsLen,
                      uint256 retValMem,
                      uint256 outputSize) private returns (bool callSuccess) {

        address dest = heads[i];                                                // 200 GAS

        assembly {
            callSuccess := call(                                                // 700 GAS
                gasForHeads,        // keep enough gas to avoid OOG
                dest,               // destination address
                0,                  // value
                argsMem,            // inputs start here
                argsLen,            // input length
                retValMem,          // outputs will be written here
                outputSize          // size of outputs
            )
        }

        return callSuccess;
    }

    /*
     * Current value of the reentrancy mutex
     */
    function getMutex() external returns(bool mutex) {
        return nextCall != 0;
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
     * Invoked if a head called a function that interacts with the blockchain.
     *
     * Call data format: The last 32 bytes of the call data represent the
     * type of callback:
     *
     *  [0 - 4]     LOGi
     *  [5]         CALL
     *  [6]         BALANCE
     *
     * The rest of the data is structured as follows
     *
     *   BALANCE     : input     [address (32), callType (32)]
     *                 output    [balance (32)]
     *
     *   CALL        : input     [sig(4), args (...), gas(32), address(32),
     *                            value(32), outputSize(32), callType(32)]
     *                 output    [outputSizeBytes(32), success value(32)]
     *
     *   LOGi        : input     [memArgs, topics(i*32), callType(32)]
     *                 output    None
     *
     *
     * If we run out of gas in a callback, this call throws. The instrumented
     * head will detect that the call failed and return -1, which `multiCall`
     * will take as an indication that the call should throw.
     */
    function headCallback() private {
        /*
         * A discrepancy was noticed in a previous callback
         * (from a different head). Just throw at this point so that the head
         * returns to `multiCall`
         */
        assert(!discrepancy);

        /*
         * Get the type of call (first word of call data) and the length of
         * the call arguments.
         */
        uint256 callbackType;

        assembly {
            callbackType := calldataload(sub(calldatasize, 0x20))
        }

        if (DEBUG_MODE) {
            InCallBack(msg.data.length, callbackType, msg.sender);
        }

        /*
         * Compute hash of callback data to compare callbacks made by each head.
         * For a CALL instruction, ignore the gas amount as different heads may
         * specify different amounts.
         */
        bytes32 hash;

        assembly {
            let x := mload(0x40)                    // get free memory

            switch callbackType
            // cannot use constants in inline assembly so hardcode CALL_CALLBACK
            case 5 {
                let l := sub(calldatasize, 0xa0)    // length of sig + args
                calldatacopy(x, 0x0, l)             // copy sig and args
                calldatacopy(add(x, l),             
                             add(l, 0x20),          // skip gas value
                             0x80)                  // addr + value +
                                                    // outputSize + callType
                hash := sha3(x, sub(calldatasize, 0x20))    
            }
            default {
                calldatacopy(x, 0x0, calldatasize)  // hash all the arguments
                hash := sha3(x, calldatasize)
            }
        }

        /*
         * If first head, set up a new storage space for this callback and
         * execute it.
         * If not first head, verify that signatures match and return output
         */

        if ( msg.sender != heads[0] ) {

            // find the call made by the first head
            // if this head is making more calls than the first head, we will
            // return arbitrary values and wait for the head to return to
            // `multiCall` where the discrepancy will be noted
            callData memory s = calls[nextCall];

            if (DEBUG_MODE) {
                RepeatCallback(callbackType, s.returnValue.length, s.hash, hash);
            }

            // there is a discrepancy in the called function
            if ( hash != s.hash ) {
                discrepancy = true;
                return;
            }
            
            // nothing to return on a LOG instruction
            if ( callbackType <= CALLBACK_TYPE_LOG ) {
                nextCall += 1;
                return;
            }

            // return the same value as for the first head
            uint256 retLen = s.returnValue.length;

            bytes memory retVal = new bytes(retLen);
            retVal = calls[nextCall].returnValue;

            nextCall += 1;

            assembly {
                // skip array length slot
                return(add(retVal, 0x20), retLen)
            }

        } else {
            // Callback from first head: make the call and store the
            // return value

            nextCall += 1;

            if ( callbackType <= CALLBACK_TYPE_LOG ) {
                logCallback(callbackType, hash);
            } else if ( callbackType == CALLBACK_TYPE_CALL ) {
                externalCallCallback(hash);
            } else if ( callbackType == CALLBACK_TYPE_BALANCE ) {
                balanceCallback(hash);
            } else {
                // Bad instrumentation: shouldn't happen
                revert();
            }
        }

        if (DEBUG_MODE) {
            CallBackDone(callbackType, msg.sender);
        }

        return;
    }

    /*
     * Handle a callback from the first head to log an event
     */
    function logCallback(uint256 numTopics, bytes32 hash) private {

        uint256 memLen;

        assembly {
            // length of the data to log
            memLen := sub(sub(calldatasize, 0x20), mul(numTopics, 0x20))

            let x := mload(0x40)            // pointer to free memory
            calldatacopy(x, 0x0, memLen)    // copy log data to memory
            mstore(0x40, add(x, memLen))    // increment free memory pointer

            switch numTopics
            case 0 { log0(x, memLen) }
            case 1 { log1(x, memLen, calldataload(memLen)) }
            case 2 { log2(x, memLen, calldataload(memLen),
                                      calldataload(add(memLen, 0x20))) }
            case 3 { log3(x, memLen, calldataload(memLen),
                                      calldataload(add(memLen, 0x20)),
                                      calldataload(add(memLen, 0x40))) }
            case 4 { log4(x, memLen, calldataload(memLen),
                                      calldataload(add(memLen, 0x20)),
                                      calldataload(add(memLen, 0x40)),
                                      calldataload(add(memLen, 0x60))) }
        }

        if (DEBUG_MODE) {
            LogCallBack(hash, numTopics, memLen);
        }

        calls[nextCall - 1].hash = hash;
        if (calls[nextCall - 1].returnValue.length != 0) {
            calls[nextCall - 1].returnValue.length = 0;
        }
        return;
    }

    /*
     * Handle a callback from the first head for a CALL instruction
     */
    function externalCallCallback(bytes32 hash) private {

        bytes4 sig;
        address destAddr;
        uint256 gasVal;
        uint256 weiVal;
        uint256 outputSize;
        uint256 argsLen;

        // get all the call data
        assembly {
            sig := calldataload(0x0)
            outputSize := calldataload(sub(calldatasize, 0x40))
            weiVal := calldataload(sub(calldatasize, 0x60))
            destAddr := calldataload(sub(calldatasize, 0x80))
            gasVal := calldataload(sub(calldatasize, 0xa0))
            argsLen := sub(calldatasize, 0xa0)
        }

        // disregard the signature
        if (argsLen > 0) {
            argsLen -= 0x4;
        }

        if (DEBUG_MODE) {
            ExternalCallBack(sig, gasVal, weiVal, destAddr, outputSize, argsLen);
        }

        calls[nextCall - 1].hash = hash;

        // allocate space for the return value and return status (32 bytes)
        bytes memory retValMem = new bytes(outputSize + 0x20);

        bool callSuccess;

        assembly {
            let x := mload(0x40)                // free memory pointer
            mstore(x, sig)                      // copy signature
            calldatacopy(add(x, 0x4),           // copy arguments into memory
                         0x4, argsLen)

            mstore(0x40,                         // reset free memory
                   add(argsLen, 0x4))

            callSuccess := call(
                gasVal,                 // give the specified amount of gas
                destAddr,               // destination address
                weiVal,                 // specified value
                x,                      // inputs start here
                add(argsLen, 0x4),      // input length (args + signature)
                add(retValMem, 0x20),   // outputs will be written here
                                        // (skip dynamic array length)
                outputSize              // size of outputs
            )
            mstore(add(add(retValMem, 0x20),
                   outputSize), callSuccess)  // save the call status

        }

        // if the call didn't succeed use the remaining gas to return the
        // failure status to the head
        if ( callSuccess ) {
            calls[nextCall - 1].returnValue = new bytes(outputSize + 0x20);
            calls[nextCall - 1].returnValue = retValMem;
        }

        assembly {
            // skip the array length slot
            return(add(retValMem, 0x20), add(outputSize, 0x20))
        }
    }

    /*
     * Handle a callback from the first head for a BALANCE instruction
     */
    function balanceCallback(bytes32 hash) private {

        address addr;
        uint256 bal;

        assembly {
            addr := calldataload(0x0)   // get address
            bal := balance(addr)        // get balance
        }

        if (DEBUG_MODE) {
            BalanceCallBack(addr, bal);
        }

        calls[nextCall - 1].hash = hash;

        bytes memory balMem = new bytes(32);

        assembly {
            mstore(add(balMem, 0x20), bal)
        }

        calls[nextCall - 1].returnValue = new bytes(32);
        calls[nextCall - 1].returnValue = balMem;

        assembly {
            let x := mload(0x40)        // get free memory
            mstore(x, bal)              // store balance
            return(x, 0x20)             // return balance
        }
    }

    /*
     * Estimate of gas cost in `multiCall`
     */
    function multiCallGasCost(uint256 callDataSize) private returns(uint256 totalGas) {
        uint256 numHeads = heads.length;

        uint256 preLoopCost = (callDataSize * 3);
        uint256 firstHeadCost = MULTI_CALL_LOOP_GAS + 200 + 5000;
        uint256 otherHeadCost = MULTI_CALL_LOOP_GAS + 400;
        uint256 postLoopCost = (MAX_RETURN_BYTES + 0x40) * 3;

        uint256 tot = preLoopCost + firstHeadCost + (numHeads - 1) * otherHeadCost + postLoopCost;
        return tot;
    }

    /*
     * Estimate of gas cost in default function
     */
    function totalGasCost(uint256 callDataSize) private returns(uint256 totalGas) {
        uint256 preCost = 700 + (callDataSize * 3);
        uint256 multiCallCost = multiCallGasCost(callDataSize + 0x4);
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
