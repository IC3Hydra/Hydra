# Solidity-Compatible ERC20 Token
# Implements https://github.com/ethereum/EIPs/issues/20
# Author: Phil Daian

# The use of the num256 datatype as in this token is not
# recommended, as it can pose security risks.

# Events are not yet supported in Viper, so events are NOT
# included in this token.  This makes this token incompatible
# with some log-only clients.

# This token is intended as a proof of concept towards
# language interoperability and not for production use.

# To maintain compatibility with both Solidity tokens and the
# existing ERC20 specification, this contract will throw
# only when a non-payable function is attempted to be called
# with some value; otherwise (on conditions like overflow),
# false will be returned.

balances: num256[address]
allowances: (num256[address])[address]
num_issued: num256

# Utility functions for overflow checking
@constant
def is_overflow_add(a : num256, b : num256) -> bool:
    result = num256_add(a, b)
    return num256_lt(result, a)

@constant
def is_overflow_sub(a : num256, b : num256) -> bool:
    return num256_lt(a, b)

@payable
def deposit():
    _value = msg.value
    _sender = msg.sender
    assert not self.is_overflow_add(self.balances[_sender], as_num256(_value))
    assert not self.is_overflow_add(self.num_issued, as_num256(_value))
    self.balances[_sender] = num256_add(self.balances[_sender], as_num256(_value))
    self.num_issued = num256_add(self.num_issued, as_num256(_value))
    # Fire deposit event
    byte_value = concat(as_bytes32(_value), "")
    raw_log([keccak256("Transfer(address,address,uint256)"), as_bytes32(0), as_bytes32(_sender)], byte_value)

def withdraw(_value : num256) -> bool:
    _sender = msg.sender
    # Make sure sufficient funds are present, op will not underflow supply
    assert not self.is_overflow_sub(self.balances[_sender], _value)
    assert not self.is_overflow_sub(self.num_issued, _value)
    self.balances[_sender] = num256_sub(self.balances[_sender], _value)
    self.num_issued = num256_sub(self.num_issued, _value)
    send(_sender, as_wei_value(as_num128(_value), wei))
    # Fire withdraw event as transfer to 0x0
    byte_value = concat(as_bytes32(_value), "")
    raw_log([keccak256("Transfer(address,address,uint256)"), as_bytes32(_sender), as_bytes32(0)], byte_value)
    return true

@constant
def totalSupply() -> num256:
    return self.num_issued

@constant
def balanceOf(_owner : address) -> num256:
    return self.balances[_owner]

def transfer(_to : address, _value : num256) -> bool:
    _sender = msg.sender
    assert not self.is_overflow_add(self.balances[_to], _value)
    assert not self.is_overflow_sub(self.balances[_sender], _value)
    self.balances[_sender] = num256_sub(self.balances[_sender], _value)
    self.balances[_to] = num256_add(self.balances[_to], _value)
    # Fire transfer event
    byte_value = concat(as_bytes32(_value), "")
    raw_log([keccak256("Transfer(address,address,uint256)"), as_bytes32(_sender), as_bytes32(_to)], byte_value)
    return true

def transferFrom(_from : address, _to : address, _value : num256) -> bool:
    _sender = msg.sender
    allowance = self.allowances[_from][_sender]
    assert not self.is_overflow_add(self.balances[_to], _value)
    assert not self.is_overflow_sub(self.balances[_from], _value)
    assert not self.is_overflow_sub(allowance, _value)
    self.balances[_from] = num256_sub(self.balances[_from], _value)
    self.balances[_to] = num256_add(self.balances[_to], _value)
    self.allowances[_from][_sender] = num256_sub(allowance, _value)
    # Fire transfer event
    byte_value = concat(as_bytes32(_value), "")
    raw_log([keccak256("Transfer(address,address,uint256)"), as_bytes32(_from), as_bytes32(_to)], byte_value)
    return true

def approve(_spender : address, _value : num256) -> bool:
    self.allowances[msg.sender][_spender] = _value
    byte_value = concat(as_bytes32(_value), "")
    raw_log([keccak256("Approval(address,address,uint256)"), as_bytes32(msg.sender), as_bytes32(_spender)], byte_value)
    return true

@constant
def allowance(_owner : address, _spender : address) -> num256:
    return self.allowances[_owner][_spender]


