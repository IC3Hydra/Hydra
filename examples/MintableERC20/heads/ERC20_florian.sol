
// https://github.com/ethereum/EIPs/issues/20
contract MintableERC20Interface {
    function totalSupply(address _sender) constant returns (uint256 totalSupply);
    function balanceOf(address _sender, address _owner) constant returns (uint256 balance);
    function transfer(address _sender, address _to, uint256 _value) returns (bool success);
    function transferFrom(address _sender, address _from, address _to, uint256 _value) returns (bool success);
    function approve(address _sender, address _spender, uint256 _value) returns (bool success);
    function allowance(address _sender, address _owner, address _spender) constant returns (uint256 remaining);
    
    function mint(address _sender, address _to, uint256 _value) returns (bool success);
    function finishMinting(address _sender) returns (bool success);
}

contract MyToken is MintableERC20Interface {
    // mapping from account address to current balance
    mapping(address => uint256) _accountBalances;
    
    // mapping from account owner to accounts allowed to withdraw 
    // specified amounts
    mapping(address => mapping(address => uint256)) _approvals;
    
    uint256 private _totalSupply = 0;

    address public creator;
    bool public mintingFinished = false;

    function MyToken() {
        creator = msg.sender;
    }

    function mint(address _sender, address _to, uint256 _value) returns (bool success) {
        if (_sender != creator) { return false; }

        if (mintingFinished) { return false; }

        // check that mint doesn't overflow total_supply
        if (_totalSupply + _value < _totalSupply) { return false; }

        _accountBalances[_to] += _value;
        _totalSupply += _value;

        return true;
    }

    function finishMinting(address _sender) returns (bool success) {
        if (_sender != creator) { return false; }
        mintingFinished = true;
        return true;
    }
    
    function totalSupply(address _sender) constant returns (uint256 total_supply) {
        return _totalSupply;
    }
    
    function balanceOf(address _sender, address _owner) constant returns (uint256 balance) {
        return _accountBalances[_owner];   
    }
    
    function transfer(address _sender, address _to, uint256 _value) returns (bool success) {
        if ( _accountBalances[_sender] >= _value  // sender has enough resources
        ){
            _accountBalances[_sender] -= _value;
            _accountBalances[_to] += _value;
            return true;
        }
        
        return false;
    }
    
    function transferFrom(address _sender, address _from, address _to, uint256 _value) returns (bool success) {
        if ( _approvals[_from][_sender] >= _value  // sender is approved to withdraw
             && _accountBalances[_from] >= _value  // origin account has enough resources
        ){
            _approvals[_from][_sender] -= _value;
            _accountBalances[_from] -= _value;
            _accountBalances[_to] += _value;
            return true;
        }
        
        return false;   
    }
    
    function approve(address _sender, address _spender, uint256 _value) returns (bool success) {
        if ( _approvals[_sender][_spender] > 0 && _value > 0 ) {
            return false;
        }

        _approvals[_sender][_spender] = _value;
        return true;
    }
    
    function allowance(address _sender, address _owner, address _spender) constant returns (uint256 remaining) {
        return _approvals[_owner][_spender];   
    }
}