

contract MyToken {
    // mapping from account address to current balance
    mapping(address => uint256) _accountBalances;
    
    // mapping from account owner to accounts allowed to withdraw 
    // specified amounts
    mapping(address => mapping(address => uint256)) _approvals;
    
    uint256 private _totalSupply = 0;

    function deposit(address _sender, uint256 _msg_value) returns (uint256[1] success) {
        // check that deposit doesn't overflow total_supply
        if(!(_totalSupply + _msg_value >= _totalSupply)){
            return ([uint256(0)]);
        }

        _accountBalances[_sender] += _msg_value;
        _totalSupply += _msg_value;
        return ([uint256(1)]);
    }
    
    function withdraw(address _sender, uint256 _msg_value, uint256 _value) returns (uint256[4] success) {
        if (_msg_value != 0) {
            return ([uint256(0), 0, 0, 0]);
        }

        if ( _accountBalances[_sender] < _value ) {
            return ([uint256(0), 0, 0, 0]);
        }

        // reduce funds BEFORE we send
        _accountBalances[_sender] -= _value;
        _totalSupply -= _value;

        return ([uint256(1), 1, uint256(_sender), _value]);
    }
    
    function totalSupply(address _sender, uint256 _msg_value) constant returns (uint256[2] total_supply) {
        if (_msg_value != 0) {
            return ([uint256(0), 0]);
        }
        return ([uint256(1), _totalSupply]);
    }

    function balanceOf(address _sender, uint256 _msg_value, address _owner) constant returns (uint256[2] balance) {
        if (_msg_value != 0) {
            return ([uint256(0), 0]);
        }

        return ([uint256(1), _accountBalances[_owner]]);
    }

    function transfer(address _sender, uint256 _msg_value, address _to, uint256 _value) returns (uint256[2] success) {
        if (_msg_value != 0) {
            return ([uint256(0), 0]);
        }

        if ( _accountBalances[_sender] >= _value  // sender has enough resources
        ){
            _accountBalances[_sender] -= _value;
            _accountBalances[_to] += _value;
            return ([uint256(1), 1]);
        }

        return ([uint256(0), 0]);
    }

    function transferFrom(address _sender, uint256 _msg_value, address _from, address _to, uint256 _value) returns (uint256[2] success) {
        if (_msg_value != 0) {
            return ([uint256(0), 0]);
        }

        if ( _approvals[_from][_sender] >= _value  // sender is approved to withdraw
             && _accountBalances[_from] >= _value  // origin account has enough resources
        ){
            _approvals[_from][_sender] -= _value;
            _accountBalances[_from] -= _value;
            _accountBalances[_to] += _value;
            return ([uint256(1), 1]);
        }

        return ([uint256(0), 0]);
    }

    function approve(address _sender, uint256 _msg_value, address _spender, uint256 _value) returns (uint256[2] success) {
        if (_msg_value != 0) {
            return ([uint256(0), 0]);
        }

        _approvals[_sender][_spender] = _value;
        return ([uint256(1), 1]);
    }

    function allowance(address _sender, uint256 _msg_value, address _owner, address _spender) constant returns (uint256[2] remaining) {
        if (_msg_value != 0) {
            return ([uint256(0), 0]);
        }

        return ([uint256(1), _approvals[_owner][_spender]]);
    }
}