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
pragma solidity ^0.4.18;

contract ASMUtils {
    function _calldataload(uint256 startPos) internal pure returns (bytes32 word) {
        assembly {
            word := calldataload(startPos)
        }
    }

    function _malloc(uint256 size) internal pure returns (uint256 memStart) {
        assembly {
            memStart := mload(0x40)
            mstore(0x40, add(memStart, size))
        }
    }

    function _mstore(uint256 memPos, bytes32 word) internal pure {
        assembly {
            mstore(memPos, word)
        }
    }

    function _mload(uint256 memPos) internal pure returns (bytes32 word) {
        assembly {
            word := mload(memPos)
        }
    }

    function _arrayMemPos(bytes b) internal pure returns (uint256 mem) {
        assembly{
            mem := b
        }
    }

    function _return(uint256 memPos, uint256 len) internal pure {
        assembly {
            return(memPos, len)
        }
    }

    function _returnVal(uint256 val) internal pure {
        assembly {
            let mem := mload(0x40)
            mstore(mem, val)
            return(mem, 32)
        }
    }

    function _calldatacopy(uint256 memPos, uint256 callDataPos, uint256 len) internal pure {
        assembly {
            calldatacopy(memPos, callDataPos, len)
        }
    }

    function _call(uint256 gasAmount, address dest, uint256 val, uint256 argsMem, uint256 argsLen) internal returns (bool callSuccess) {
        assembly {
            callSuccess := call(gasAmount, dest, val, argsMem, argsLen, 0x0, 0x0)
        }
    }

    function _returndatasize() internal pure returns (uint256 size) {
        assembly {
            size := returndatasize
        }
    }

    function _returndatacopy(uint256 memPos, uint256 retDataPos, uint256 len) internal pure {
        assembly {
            returndatacopy(memPos, retDataPos, len)
        }
    }

    function _keccak256(uint256 memPos, uint256 len) internal pure returns (bytes32 hash) {
        assembly {
            hash := keccak256(memPos, len)
        }
    }

    function _getReturnData() internal pure returns (uint256 memPos) {
        uint256 retSize;

        assembly {
            retSize := returndatasize
        }

        memPos = _malloc(retSize);
        _returndatacopy(memPos, 0, retSize);
        return memPos;
    }

    function _revert(uint256 memPos, uint256 len) internal pure {
        assembly {
            revert(memPos, len)
        }
    }

    function _revertVal(uint256 val) internal pure {
        assembly {
            let mem := mload(0x40)
            mstore(mem, val)
            revert(mem, 32)
        }
    }
}

contract HydraContract is ASMUtils {

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

    // Signature of the `multiCall` function
    bytes4 constant MULTI_CALL_SIG = bytes4(keccak256("multiCall()"));

    // Values used to differentiate between the type of head callbacks
    uint256 CALLBACK_TYPE_LOG = 4;
    uint256 CALLBACK_TYPE_CALL = 5;
    uint256 CALLBACK_TYPE_BALANCE = 6;

    // set to `true` to have the meta contract log events
    bool constant DEBUG_MODE /*__JINJA_TEMPLATE__;= {{ 'true' if debug else 'false' }}; /*__JINJA_TEMPLATE__*/ = false;
    /* ------- end constant vars ------- */


    /*
     * CONSTRUCTOR
     * Takes as argument the bounty value in WEI
     */
    function HydraContract() public payable {
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

        /*
         * check if this is a callback from one of the heads.
         * MUST BE PLACED AFTER THE RE-ENTRANCY LOCK CHECK
         */
        for (uint i = 0; i < heads.length; i++) {
            if (msg.sender == heads[i]) {
                headCallback();
                return;
            }
        }

        // non-reentrancy lock
        assert(nextCall == 0);

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
        bool isHydraInitCall = false;

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

        /*
         * arguments for `multiCall()`
         */
        uint256 mem = _malloc(msg.data.length + 4 + 32 + 32);
        _mstore(mem, bytes32(MULTI_CALL_SIG));
        _mstore(mem + 4, bytes32(msg.sender));
        _mstore(mem + 4 + 32, bytes32(msg.value));
        _calldatacopy(mem + 4 + 32 + 32, 0, msg.data.length);

        bool callSuccess = _call(msg.gas, this, 0, mem, msg.data.length + 4 + 32 + 32);

        uint256 retSize = _returndatasize();

        if ( !callSuccess ) {
            if ( retSize > 0 ) {
                // If we're here, `multiCall` reverted to indicate a discrepancy

                if (DEBUG_MODE) {
                    MultiCallReturn(sig, 0x0, callSuccess);                         // 4 * 375 GAS
                }

                payBounty();                                                        // PAYBOUNTY_GAS
                return;
            } else {
                // multiCall threw an exception (OOG, stack, etc)
                revert();
            }
        }

        if (DEBUG_MODE) {
            MultiCallReturn(sig, retSize, callSuccess);
        }

        uint256 retMem = _getReturnData();

        /*
         * If the heads agree that the call should throw, `multiCall`
         * returns -1
         */
        if ( _mload(retMem) == bytes32(~0) ) {
            _revert(retMem + 32, retSize - 32);
        }

        /*
         * Reset all temporary storage used to get a gas reimbursement.
         * No need to reset `discrepancy` as `multiCall` throws when it is set
         */
        for (i=0; i<nextCall; i++) {
            delete calls[i].hash;
            if (calls[i].returnValue.length > 0) {
                delete calls[i].returnValue;
            }
        }

        nextCall = 0;

        // return the actual output
        if (retSize > 32) {
            _return(retMem + 32, retSize - 32);
        } else {
            return;
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

        // the hash of the output from the currently evaluated head.
        bytes32 retHash;

        // the hash of the output produced by the first head.
        bytes32 firstHeadRetHash;

        // the success flag for the call to the head
        bool callSuccess;

        // memory location and size for the heads' return value
        uint256 retMem;
        uint256 retSize;

        /*
         * Copy the call arguments into memory
         * The arguments received from the fallback function are:
         * [multiCallSig(0x4) restOfArgs(calldatasize - 0x4)]
         */
        uint256 mem = _malloc(msg.data.length - 4);
        _mstore(mem, bytes32(MULTI_CALL_SIG));
        _calldatacopy(mem, 4, msg.data.length - 4);

        // keep track of number of calls made by first head
        uint256 numCallsFirstHead = 0;

        // execute all heads one after the other
        for (uint i = 0; i < heads.length; ++i) {                               // 200 GAS

            // reset the storage value used to keep track of the next
            // `CallData` value to use to handle a callback
            if (nextCall != 0) {                                                // 200 GAS
                nextCall = 0;                                                   // 5000 GAS
            }

            callSuccess = _call(msg.gas, heads[i], 0, mem, msg.data.length - 4);

            // if the CALL didn't succeed (probably OOG),
            // throw so the default function will throw
            if ( !callSuccess ) {
                revert();
            }

            retMem = _getReturnData();
            retSize = _returndatasize();

            if (DEBUG_MODE) {
                HeadReturn(retSize, callSuccess);                               // 3 * 375 GAS
            }

            if ( _mload(retMem) == bytes32(~0) ) {
                // head signals a throw by returning -1
                retHash = bytes32(retSize);
            } else {
                // compute H(retMem)
                retHash = _keccak256(retMem, retSize);
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
                    _revertVal(uint(~0));
                }

                if ( _mload(retMem) != bytes32(~0) &&
                    (discrepancy || nextCall != numCallsFirstHead) ) {          // 400 GAS
                    // some discrepancy in the type or number of callbacks
                    // wrapper will pay bounty
                    _revertVal(uint(~0));
                }
            }
        }

        _return(retMem, retSize);
    }

    /*
     * Current value of the reentrancy mutex
     */
    function getMutex() external view returns(bool mutex) {
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
        msg.sender.transfer(bounty);                                            //  9700 GAS
        creator.transfer(rest);                                                     //  9700 GAS
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
         * Get the type of call (last byte of call data)
         */
        uint256 callbackType = uint256(msg.data[msg.data.length - 1]);

        if (DEBUG_MODE) {
            InCallBack(msg.data.length, callbackType, msg.sender);
        }

        /*
         * Compute hash of callback data to compare callbacks made by each head.
         * For a CALL instruction, ignore the gas amount as different heads may
         * specify different amounts.
         */

        uint256 mem = _malloc(msg.data.length);
        _calldatacopy(mem, 0, msg.data.length);

        // overwrite gas amount with 0
        if (callbackType == CALLBACK_TYPE_CALL) {
            _mstore(mem + msg.data.length - 5*32, 0);
        }

        // hash of callback data
        bytes32 hash = _keccak256(mem, msg.data.length);

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
                return(add(retVal, 32), retLen)
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
     *
     * LOGi        : input     [memArgs, topics (i*32), 0-4 (32)]
     *               output    None
     */
    function logCallback(uint256 numTopics, bytes32 hash) private {

        // length of the data to log
        uint256 memLen = msg.data.length - 32 * (numTopics + 1);

        uint256 mem = _malloc(memLen);
        _calldatacopy(mem, 0, memLen);

        assembly {
            switch numTopics
            case 0 { log0(mem, memLen) }
            case 1 { log1(mem, memLen, calldataload(memLen)) }
            case 2 { log2(mem, memLen, calldataload(memLen),
                                      calldataload(add(memLen, 0x20))) }
            case 3 { log3(mem, memLen, calldataload(memLen),
                                      calldataload(add(memLen, 0x20)),
                                      calldataload(add(memLen, 0x40))) }
            case 4 { log4(mem, memLen, calldataload(memLen),
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
     *
     *   CALL        : input     [sig(4), args (...), gas(32), address(32),
     *                            value(32), outputSize(32), 5]
     *                 output    [outputSizeBytes(32), success value(32)]
     */
    function externalCallCallback(bytes32 hash) private {

        uint256 mem = _malloc(msg.data.length - 32);
        _calldatacopy(mem, 0, msg.data.length - 32);

        bytes4 sig = bytes4(_mload(mem));
        uint256 outputSize = uint256(_mload(mem + msg.data.length - 2 * 32));
        uint256 weiVal = uint256(_mload(mem + msg.data.length - 3 * 32));
        address destAddr = address(_mload(mem + msg.data.length - 4 * 32));
        uint256 gasVal = uint256(_mload(mem + msg.data.length - 5 * 32));
        uint256 argsLen = msg.data.length - 5 * 32;

        if (DEBUG_MODE) {
            ExternalCallBack(sig, gasVal, weiVal, destAddr, outputSize, argsLen);
        }

        calls[nextCall - 1].hash = hash;

        bool callSuccess = _call(gasVal, destAddr, weiVal, mem, argsLen);

        // allocate space for the return value and return status (32 bytes)
        bytes memory retValMem = new bytes(_returndatasize() + 32);
        bytes32 callSuccessBytes = callSuccess ? bytes32(1) : bytes32(0);
        _returndatacopy(_arrayMemPos(retValMem) + 32, 0, _returndatasize());
        _mstore(_arrayMemPos(retValMem) + 32 + _returndatasize(), callSuccessBytes);

        // if the call didn't succeed use the remaining gas to return the
        // failure status to the head
        if ( callSuccess ) {
            calls[nextCall - 1].returnValue = new bytes(_returndatasize() + 32);
            calls[nextCall - 1].returnValue = retValMem;
        }

        _return(_arrayMemPos(retValMem) + 32, _returndatasize() + 32);
    }

    /*
     * Handle a callback from the first head for a BALANCE instruction
     *
     *   BALANCE     : input     [address (32), 6]
     *                 output    [balance (32)]
     */
    function balanceCallback(bytes32 hash) private {

        uint256 mem = _malloc(32);
        _calldatacopy(mem, 0, 32);

        address addr = address(_mload(mem));
        uint256 bal = addr.balance;

        if (DEBUG_MODE) {
            BalanceCallBack(addr, bal);
        }

        calls[nextCall - 1].hash = hash;

        bytes memory balMem = new bytes(32);
        _mstore(_arrayMemPos(balMem) + 32, bytes32(bal));

        calls[nextCall - 1].returnValue = new bytes(32);
        calls[nextCall - 1].returnValue = balMem;

        _return(_arrayMemPos(balMem) + 32, 32);
    }
}
