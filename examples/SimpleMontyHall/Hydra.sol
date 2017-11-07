
pragma solidity ^0.4.18;

contract Head {
    function HYDRA_INIT(address _sender) public;
}

contract ASMUtils {

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

    function _return(uint256 memPos, uint256 len) internal pure {
        assembly {
            return(memPos, len)
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
        uint256 retSize = _returndatasize();

        memPos = _malloc(retSize);
        _returndatacopy(memPos, 0, retSize);
        return memPos;
    }
}

contract SimpleMontyHall is ASMUtils {

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

    /* ------- end constant vars ------- */


    /*
     * CONSTRUCTOR
     * Takes as argument the bounty value in WEI
     */
    function HydraContract() public payable {
        bountyValue = msg.value;
        creator = msg.sender;
    }

    function HYDRA_INIT() public {
        for (uint i=0; i< heads.length; i++) {
            Head(heads[i]).HYDRA_INIT(msg.sender);
        }
    }

    /* 
     * On an external call: dispatch the call to `multiCall`, specifying the
     *   message sender as the first argument.
     */
    function() public payable {

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
        
        if ( sig == bytes4(keccak256("InitMonty(int128,int128,bytes32)")) ) {
            newSig = bytes4(keccak256("InitMonty(address,uint256,int128,int128,bytes32)"));
            outputSize = 0x0;

        } else if ( sig == bytes4(keccak256("PlayMontyRound1(int128,int128)")) ) {
            newSig = bytes4(keccak256("PlayMontyRound1(address,uint256,int128,int128)"));
            outputSize = 0x0;

        } else if ( sig == bytes4(keccak256("OpenDoors(int128,int128)")) ) {
            newSig = bytes4(keccak256("OpenDoors(address,uint256,int128,int128)"));

        } else if ( sig == bytes4(keccak256("isOpened(int128,int128)")) ) {
            newSig = bytes4(keccak256("isOpened(address,uint256,int128,int128)"));

        } else if ( sig == bytes4(keccak256("PlayMontyRound2(int128,int128)")) ) {
            newSig = bytes4(keccak256("PlayMontyRound2(address,uint256,int128,int128)"));

        } else if ( sig == bytes4(keccak256("EndGame(int128,bytes32,int128)")) ) {
            newSig = bytes4(keccak256("EndGame(address,uint256,int128,bytes32,int128)"));

        } else if ( sig == bytes4(keccak256("Payout(bool,int128)")) ) {
            newSig = bytes4(keccak256("Payout(address,uint256,bool,int128)"));

        } else if ( sig == bytes4(keccak256("EscapeHatch()")) ) {
            newSig = bytes4(keccak256("EscapeHatch(address,uint256)"));

        } else if ( sig == bytes4(keccak256("RefundInactive(int128)")) ) {
            newSig = bytes4(keccak256("RefundInactive(address,uint256,int128)"));

        } else if ( sig == bytes4(keccak256("RefundAfterEscapeHatch(int128)")) ) {
            newSig = bytes4(keccak256("RefundAfterEscapeHatch(address,uint256,int128)"));

        } else {
            revert();
        }

        // add the sender and value to the call args
        uint256 mem = _malloc(msg.data.length + 64);
        _mstore(mem, bytes32(newSig));
        _mstore(mem + 4, bytes32(msg.sender));
        _mstore(mem + 4 + 32, bytes32(msg.value));
        _calldatacopy(mem + 4 + 32 + 32, 4, msg.data.length - 4);

        var (callSuccess, retValMem) = multiCall(mem, msg.data.length + 64, outputSize);

        // if multiCall returns `false`, a discrepancy has been found
        if (!callSuccess) {
            payBounty();
            return;
        }

        // return the heads' output
        _return(retValMem, outputSize);
    }

    function multiCall(uint256 args, uint256 argsLen, uint256 outputSize) internal returns (bool success, uint256 retValMem) {

        // the hash of the output from the currently evaluated head.
        bytes32 retHash;

        // the hash of the output produced by the first head.
        bytes32 firstHeadRetHash;

        // execute all heads one after the other
        for (uint i = 0; i < heads.length; ++i) {

            // call the head. skip the signature of `multiCall` in the args
            require(_call(msg.gas, heads[i], 0, args, argsLen));

            retValMem = _getReturnData();

            // get the hash of the return value
            retHash = _keccak256(retValMem, _returndatasize());

            // first head
            if ( i == 0) {

                // save hash of output of first heads
                firstHeadRetHash = retHash;
            } else {

                if ( retHash != firstHeadRetHash ) {
                    // discrepancy between heads' outputs or throw behavior
                    return (false, 0);
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

        // if the head signals a throw, throw here
        if (_mload(retValMem) == 0x0) {
            revert();
        }

        uint256 retValSize = _returndatasize();

        address dest;
        uint256 val;
        for (uint j = 32 + outputSize; j<retValSize; j+=64) {
            dest = address(_mload(retValMem + j));
            val = uint256(_mload(retValMem + j + 32));

            if (val != 0 && !dest.send(val)) {
                revert();
            }
        }

        // skip call success status
        return (true, retValMem + 32);
    }

    /*
     * Pays out the bounty when a discrepancy between heads is found
     */
    function payBounty() private {

        if (DEBUG_MODE) {
            BountyPayed();
        }

        uint256 bal = this.balance - bountyValue;
        uint256 bounty = bountyValue + bal / 10;
        uint256 rest = this.balance - bounty;

        bountyClaimed = true;
        msg.sender.transfer(bounty);
        creator.transfer(rest);
    }
}
