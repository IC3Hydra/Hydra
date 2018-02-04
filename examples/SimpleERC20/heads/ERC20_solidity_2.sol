pragma solidity ^0.4.18;

contract Token {
    // owner -> amount
    mapping(address => uint256) balances;
    // owner -> spender -> max amount
    mapping(address => mapping(address => uint256)) allowances;
    
    uint256 supply;

    uint256 constant LOG0 = 0;
    uint256 constant LOG1 = 1;
    uint256 constant LOG2 = 2;
    uint256 constant LOG3 = 3;
    uint256 constant LOG4 = 4;
    uint256 constant SEND = 5;

    uint256 constant TRANSFER = uint256(keccak256("Transfer(address,address,uint256)"));
    uint256 constant APPROVAL = uint256(keccak256("Approval(address,address,uint256)"));

    // Deposits ether with the contract and converts it to tokens.
    // One wei is worth one token.
    function deposit(address _sender, uint256 _msg_value) public returns (uint256[6] success){
        // overflow check. not necessary for 1-to-1 token-to-wei peg, but
        // might be good to have in case somebody decides to modify this code
        // to have the owner issue tokens, etc...
        if (supply + _msg_value < supply) {
            return ([uint256(0), 0, 0, 0, 0, 0]);
        }
        supply += _msg_value;
        balances[_sender] += _msg_value;
        return ([uint256(1), LOG3, TRANSFER, 0, uint256(_sender), _msg_value]);
    }
    
    // Converts tokens to ether and withdraws the ether. 
    // One token is worth one wei.
    function withdraw(address _sender, uint256 _msg_value, uint256 _value) public returns (uint256[10] success) {
        if (_msg_value != 0) {
            return ([uint256(0), 0, 0, 0, 0, 0, 0, 0, 0, 0]);
        }

        if (_value <= balances[_sender]) {
            balances[_sender] -= _value;
            supply -= _value;
            return ([uint256(1), 1, SEND, uint256(_sender), _value, LOG3, TRANSFER, uint256(_sender), 0, _value]);
        } else {
            return ([uint256(0), 0, 0, 0, 0, 0, 0, 0, 0, 0]);
        }
    }
    
    // Spec: Get the total token supply
    function totalSupply(address, uint256 _msg_value) public constant returns (uint256[2] success) {
        if (_msg_value != 0) {
            return ([uint256(0), 0]);
        }
        return ([uint256(1), supply]);
    }

    // Spec: Get the account balance of another account with address _owner
    // The spec is a bit surprising to me. Why should this only work for the
    // balance of "another account", i.e. only if _owner != _sender?
    // For now, I am assuming that this is just due to unclear wording and that
    // anybody's balance may be queried this way.
    function balanceOf(address, uint256 _msg_value, address _owner) public constant returns (uint256[2] success) {
        if (_msg_value != 0) {
            return ([uint256(0), 0]);
        }
        return ([uint256(1), balances[_owner]]);
    }
    
    function internalTransfer(address _from, address _to, uint256 _value) internal returns (uint256[7] success) {
        if (_value <= balances[_from]) {
            balances[_from] -= _value;
            balances[_to] += _value;
            return ([uint256(1), 1, LOG3, TRANSFER, uint256(_from), uint256(_to), _value]);
        } else {
            return ([uint256(0), 0, 0, 0, 0, 0, 0]);
        }
    }
    
    // Spec: Send _value amount of tokens to address _to
    function transfer(address _sender, uint256 _msg_value, address _to, uint256 _value) public returns (uint256[7] success) {
        if (_msg_value != 0) {
            return ([uint256(0), 0, 0, 0, 0, 0, 0]);
        }

        address _from = _sender;
        return internalTransfer(_from, _to, _value);
    }
    
    // Spec: Send _value amount of tokens from address _from to address _to
    function transferFrom(address _sender, uint256 _msg_value, address _from, address _to, uint256 _value) public returns (uint256[7] success) {
        if (_msg_value != 0) {
            return ([uint256(0), 0, 0, 0, 0, 0, 0]);
        }

        address _spender = _sender;
        if(_value <= allowances[_from][_spender]) {
            uint256[7] memory ret = internalTransfer(_from, _to, _value);

            if (ret[0] == 1) {
                allowances[_from][_spender] -= _value;
                return ret;
            }
            return ([uint256(0), 0, 0, 0, 0, 0, 0]);

        }
        return ([uint256(0), 0, 0, 0, 0, 0, 0]);
    }
    
    // Spec: Allow _spender to withdraw from your account, multiple times, up 
    // to the _value amount. If this function is called again it overwrites the 
    // current allowance with _value.
    function approve(address _sender, uint256 _msg_value, address _spender, uint256 _value) public returns (uint256[7] success) {
        if (_msg_value != 0) {
            return ([uint256(0), 0, 0, 0, 0, 0, 0]);
        }

        address _owner = _sender;
        allowances[_owner][_spender] = _value;
        return ([uint256(1), 1, LOG3, APPROVAL, uint256(_sender), uint256(_spender), _value]);
    }
    
    // Spec: Returns the amount which _spender is still allowed to withdraw 
    // from _owner.
    // What if the allowance is higher than the balance of the owner? 
    // Callers should be careful to use min(allowance, balanceOf) to make sure
    // that the allowance is actually present in the account!
    function allowance(address, uint256 _msg_value, address _owner, address _spender) public constant returns (uint256[2] success) {
        if (_msg_value != 0) {
            return ([uint256(0), 0]);
        }

        return ([uint256(1), allowances[_owner][_spender]]);
    }
}
