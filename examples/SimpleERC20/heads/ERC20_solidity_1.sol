pragma solidity ^0.4.18;

contract MyToken {
    // mapping from account address to current balance
    mapping(address => uint256) _accountBalances;
    
    // mapping from account owner to accounts allowed to withdraw 
    // specified amounts
    mapping(address => mapping(address => uint256)) _approvals;
    
    uint256 private _totalSupply = 0;

    uint256 constant LOG0 = 0;
    uint256 constant LOG1 = 1;
    uint256 constant LOG2 = 2;
    uint256 constant LOG3 = 3;
    uint256 constant LOG4 = 4;
    uint256 constant SEND = 5;

    uint256 constant TRANSFER = uint256(keccak256("Transfer(address,address,uint256)"));
    uint256 constant APPROVAL = uint256(keccak256("Approval(address,address,uint256)"));

    function deposit(address _sender, uint256 _msg_value) public returns (uint256[6] success) {
        // check that deposit doesn't overflow total_supply
        if(!(_totalSupply + _msg_value >= _totalSupply)){
            return ([uint256(0), 0, 0, 0, 0, 0]);
        }

        _accountBalances[_sender] += _msg_value;
        _totalSupply += _msg_value;

        // return [throw_status, Log3("Transfer", 0, msg.sender, msg.value)]
        return ([uint256(1), LOG3, TRANSFER, 0, uint256(_sender), _msg_value]);
    }
    
    function withdraw(address _sender, uint256 _msg_value, uint256 _value) public returns (uint256[10] success) {
        if (_msg_value != 0) {
            return ([uint256(0), 0, 0, 0, 0, 0, 0, 0, 0, 0]);
        }

        if ( _accountBalances[_sender] < _value ) {
            return ([uint256(0), 0, 0, 0, 0, 0, 0, 0, 0, 0]);
        }

        // reduce funds BEFORE we send
        _accountBalances[_sender] -= _value;
        _totalSupply -= _value;

        // return [throw_status, ret_val, SEND(sender, value), Log3("Transfer", msg.sender, 0, value)]
        return ([uint256(1), 1, SEND, uint256(_sender), _value, LOG3, TRANSFER, uint256(_sender), 0, _value]);
    }
    
    function totalSupply(address, uint256 _msg_value) public constant returns (uint256[2] total_supply) {
        if (_msg_value != 0) {
            return ([uint256(0), 0]);
        }
        return ([uint256(1), _totalSupply]);
    }

    function balanceOf(address, uint256 _msg_value, address _owner) public constant returns (uint256[2] balance) {
        if (_msg_value != 0) {
            return ([uint256(0), 0]);
        }

        return ([uint256(1), _accountBalances[_owner]]);
    }

    function transfer(address _sender, uint256 _msg_value, address _to, uint256 _value) public returns (uint256[7] success) {
        if (_msg_value != 0) {
            return ([uint256(0), 0, 0, 0, 0, 0, 0]);
        }

        if ( _accountBalances[_sender] >= _value  // sender has enough resources
        ){
            _accountBalances[_sender] -= _value;
            _accountBalances[_to] += _value;

            // return [throw_status, ret_val, Log3("Transfer", msg.sender, _to, _value]
            return ([uint256(1), 1, LOG3, TRANSFER, uint256(_sender), uint256(_to), _value]);
        }

        return ([uint256(0), 0, 0, 0, 0, 0, 0]);
    }

    function transferFrom(address _sender, uint256 _msg_value, address _from, address _to, uint256 _value) public returns (uint256[7] success) {
        if (_msg_value != 0) {
            return ([uint256(0), 0, 0, 0, 0, 0, 0]);
        }

        if ( _approvals[_from][_sender] >= _value  // sender is approved to withdraw
             && _accountBalances[_from] >= _value  // origin account has enough resources
        ){
            _approvals[_from][_sender] -= _value;
            _accountBalances[_from] -= _value;
            _accountBalances[_to] += _value;

            // return [throw_status, ret_val, Log3("Transfer", _from, _to, _value]
            return ([uint256(1), 1, LOG3, TRANSFER, uint256(_from), uint256(_to), _value]);
        }

        return ([uint256(0), 0, 0, 0, 0, 0, 0]);
    }

    function approve(address _sender, uint256 _msg_value, address _spender, uint256 _value) public returns (uint256[7] success) {
        if (_msg_value != 0) {
            return ([uint256(0), 0, 0, 0, 0, 0, 0]);
        }

        _approvals[_sender][_spender] = _value;

        // return [throw_status, ret_val, Log3("Approval", msg.sender, _spender, _value)]
        return ([uint256(1), 1, LOG3, APPROVAL, uint256(_sender), uint256(_spender), _value]);
    }

    function allowance(address, uint256 _msg_value, address _owner, address _spender) public constant returns (uint256[2] remaining) {
        if (_msg_value != 0) {
            return ([uint256(0), 0]);
        }

        return ([uint256(1), _approvals[_owner][_spender]]);
    }
}