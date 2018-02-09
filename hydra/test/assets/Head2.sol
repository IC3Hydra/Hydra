

/*
Function signatures:
c1dc5e8e: HYDRA_INIT(address,address)
4ac729b8: callback_discrepancy_but_throws()
0de8cdfa: explicit_throw()
9c2d31c5: external_call(uint256)
383776fc: external_call_reentrant(uint256)
9d69a5bf: external_call_wrong_args(uint256)
030a564c: external_call_wrong_num(uint256)
b8ba86f1: external_call_wrong_val(uint256)
1839d9df: external_call_throws()
c1cfb99a: get_balance()
daef204e: instrumented_throw()
1e0e3746: log_event()
93044b4d: mul_by_hundred(uint256)
16a5637f: not_in_abi()
13cf6673: return_constant()
50345308: return_one_or_two()
df01c983: return_or_throw()
cba5188d: return_stop()
59fdfdf1: second_head_makes_call()
1b960b82: throw_discrepancy()
*/

contract Head {   
    address externalAddress;
    address metaContract;
    
    function HYDRA_INIT(address a, address b) {
        address ead;
        address mc;
        address msg_sender;

        assembly {
            msg_sender := calldataload(0x0)
            ead := calldataload(0x44)
            mc := calldataload(0x64)
        }

        Initialized2(msg_sender, ead, mc);

        externalAddress = ead;
        metaContract = mc;
    }

    event InFallback2(uint256 calldatasize, address msg_sender, uint256 msg_value, bytes4 sig, uint256 arg1);
    event Initialized2(address sender, address ead, address mc);
    event Balance2(address a, uint256 b);
    event ExternalCall2(address a, uint256 ret, bool success);
    event LogCallBack2(uint256 a, uint256 b);

    function() payable {

        uint256 callDataSize;
        bytes4 sig;
        address msg_sender;
        uint256 msg_value;
        uint256 arg1;

        assembly {
            callDataSize := calldatasize

            let x := mload(0x40)
            calldatacopy(x, 0x0, 0x20)
            msg_sender := mload(x)

            calldatacopy(x, 0x20, 0x20)
            msg_value := mload(x)

            calldatacopy(x, 0x40, 0x4)
            sig := mload(x)

            calldatacopy(x, 0x44, 0x20)
            arg1 := mload(x)
        }

        InFallback2(callDataSize, msg_sender, msg_value, sig, arg1);

        // HYDRA_INIT
        if ( sig == 0xc1dc5e8e ) {
            HYDRA_INIT(0x0, 0x0);
            return;
        }

        if ( sig == 0x13cf6673 ) {
            _return_constant();

        } else if ( sig == 0x93044b4d ) {
            _mul_by_hundred();

        } else if ( sig == 0x50345308 ) {
            _return_one_or_two();

        } else if ( sig == 0xcba5188d ) {
            _return_stop();

        } else if ( sig == 0xdf01c983 ) {
            _return_or_throw();

        } else if ( sig == 0x0de8cdfa ) {
            _explicit_throw();

        } else if ( sig == 0xc1cfb99a ) {
            _get_balance();

        } else if ( sig == 0x9c2d31c5 ) {   // external_call to `mul_by_100`
            _external_call(0xf0fff0c9, 5, arg1, 0);

        } else if ( sig == 0x9d69a5bf ) {   // external_call_wrong_args
            // Head1 calls with arg1
            _external_call(0xf0fff0c9, 5, arg1 - 1, 0);

        } else if ( sig == 0x030a564c ) {   // external_call_wrong_num
            // Head1 calls with num=5
            _external_call(0xf0fff0c9, 4, arg1, 0);

        } else if ( sig == 0xb8ba86f1 ) {  // external_call_wrong_val
            // Head1 calls with val=0
            _external_call(0xf0fff0c9, 5, arg1, 1);

        } else if ( sig == 0x383776fc  ) {  // external_call_reentrant
            _external_call(0x8b61622f, 1, arg1, 0);

         } else if ( sig == 0x1839d9df ) {  // external_call_throws
            _external_call(0x00414f8c, 1, arg1, 0);

        } else if ( sig == 0x1e0e3746 ) {
            _log_event();

        } else if ( sig == 0xdaef204e ) {
            _instrumented_throw();

        } else if ( sig == 0x1b960b82 ) {
            _throw_discrepancy();

        } else if ( sig == 0x59fdfdf1 ) {   // second head calls
            _log_event();

        } else if ( sig == 0x4ac729b8 ) {   // callback discrepancy before throw
            _log_event();
            _instrumented_throw();
        }
    }

    // return constant
    function _return_constant() internal {
        bytes4 ret = 0xabababab;

        assembly {
            let x := mload(0x40)
            mstore(x, 0x20)
            mstore(add(x, 0x20), ret)
            return(x, 0x40)
        }
    }

    // return first argument multiplied by 100
    function _mul_by_hundred() internal {
        assembly {
            let x := mload(0x40)

            // fetch first arg (after msg_sender, msg_value, sig)
            calldatacopy(x, 0x44, 0x20)
            let a := mload(x)

            mstore(x, 0x20)
            mstore(add(x, 0x20), mul(a, 100))
            return(x, 0x40)
        }
    }

    // return 2 (discrepancy with head1)
    function _return_one_or_two() internal {
        assembly {
            let x := mload(0x40)
            mstore(x, 0x20)
            mstore(add(x, 0x20), 0x2)
            return(x, 0x40)
        }
    }

    // return nothing
    function _return_stop() internal {
        assembly {
            stop
        }
    }

    // throws (head1 returns 1)
    function _return_or_throw() internal {
        throw;
    }

    // explicit throw
    function _explicit_throw() internal {
        throw;
    }

    // balance callback
    function _get_balance() internal {
        uint256 b;
        address ead = externalAddress;
        address dest = metaContract;

        assembly {
            let x := mload(0x40)                        // free memory pointer
            mstore(x, ead)                              // address
            mstore(add(x, 0x20), 0x6)                   // BALANCE TYPE

            let call_success := call(gas,                // give all the gas
                                 dest,                  // destination address
                                 0,                     // value
                                 x,                     // inputs start here
                                 0x40,                  // input length
                                 x,                     // outputs will be written here
                                 0x20)                  // size of outputs

            b := mload(x)
        }

        Balance2(ead, b);

        assembly {
            let x := mload(0x40)
            mstore(x, 0x20)
            mstore(add(x, 0x20), b)
            return(x, 0x40)
        }
    }

    function _external_call(bytes4 sig, uint256 num_calls, uint256 arg, uint256 val) internal {
        bool call_success;
        address ead = externalAddress;
        address dest = metaContract;
        bytes memory ret_val_mem;
        uint256 ret_val;
        
        for (uint8 i = 0; i < num_calls; i++ ) {
            assembly {
                ret_val_mem := mload(0x40)                  // free memory pointer
                mstore(ret_val_mem, 0x20)                   // output size
                mstore(0x40, add(ret_val_mem, 0x40))        // space for output size, outputs, call_success

                let x := mload(0x40)                        // free memory pointer
                mstore(x, sig)                              // signature of `my_function(uint256)`
                mstore(add(x, 0x4), arg)                    // args
                mstore(add(x, 0x24), gas)                   // gas
                mstore(add(x, 0x44), ead)                   // address
                mstore(add(x, 0x64), 0)                     // value
                mstore(add(x, 0x84), 0x20)                  // output size
                mstore(add(x, 0xa4), 0x5)                   // call type

                call_success := call(gas,                   // give all the gas
                                     dest,                  // destination address
                                     val,                   // value
                                     x,                     // inputs start here
                                     0xc4,                  // input length
                                     add(ret_val_mem, 0x20),// outputs will be written here
                                     0x40)                  // size of outputs

                call_success := mload(add(ret_val_mem, 0x20))
                ret_val := mload(ret_val_mem)
            }
        }

        ExternalCall2(ead, ret_val, call_success);

        assembly {
            return(ret_val_mem, 0x40)   //output size and output value
        }
    }

    function _log_event() internal {

        bytes32 log_sig = sha3("LogCallBack1(uint256,uint256)");

        address dest = metaContract;

        assembly {
            let x := mload(0x40)                        // free memory pointer
            mstore(x, 0x1)                              // log data
            mstore(add(x, 0x20), 0x2)                   // more log data
            mstore(add(x, 0x40), log_sig)               // one topic
            mstore(add(x, 0x60), 0x1)                     // log1

            let call_success := call(gas,               // give all the gas
                                     dest,              // destination address
                                     0,                 // value
                                     x,                 // inputs start here
                                     0x80,              // input length
                                     x,                 // outputs will be written here
                                     0x0)               // size of outputs
        }
    }

    // an instrumented "throw"
    function _instrumented_throw() internal {
        uint throw_val = uint(-1);
        assembly {
            let x := mload(0x40)
            mstore(x, not(0))
            return(x, 0x20)
        }
    }

    // a "throw" discrepancy (head1 throws, head2 returns constant)
    function _throw_discrepancy() internal {
        bytes4 ret = 0xabababab;

        assembly {
            let x := mload(0x40)
            mstore(x, 0x20)
            mstore(add(x, 0x20), ret)
            return(x, 0x40)
        }
    }

    // dummy function declarations so that they are included in the ABI
    function return_constant() returns (bytes4 ret) {}
    function mul_by_hundred(uint256 a) returns (uint256 ret) {} 
    function return_one_or_two() returns (uint256 ret) {}
    function return_stop() {}
    function return_or_throw() {}
    function explicit_throw() {}
    function get_balance() returns (uint256 ret) {}
    function external_call(uint256 a) returns (uint256 ret) {}
    function external_call_wrong_num(uint256 a) returns (uint256 ret) {}
    function external_call_wrong_args(uint256 a) returns (uint256 ret) {}
    function external_call_wrong_val(uint256 a) returns (uint256 ret) {}
    function external_call_reentrant(uint256 a) returns (int256 ret) {}
    function external_call_throws() returns (uint256 ret) {}
    function log_event() {}
    function second_head_makes_call() {}
    function callback_discrepancy_but_throws() {}
    function instrumented_throw() {}
    function throw_discrepancy() {}
}