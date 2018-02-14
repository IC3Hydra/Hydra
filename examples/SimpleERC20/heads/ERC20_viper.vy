# Solidity-Compatible ERC20 Token
# Implements https://github.com/ethereum/EIPs/issues/20

# The use of the num256 datatype as in this token is not
# recommended, as it can pose security risks.

# Events are not yet supported in Vyper, so events are NOT
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
@private
@constant
def is_overflow_add(a : num256, b : num256) -> bool:
    result:num256 = num256_add(a, b)
    return num256_lt(result, a)

@private
@constant
def is_overflow_sub(a : num256, b : num256) -> bool:
    return num256_lt(a, b)

@public
def deposit(_sender : address, _msg_value : num256) -> num256[6]:
    _value:num256 = _msg_value

    _fail:num256[6] = [convert(0, 'num256'), convert(0, 'num256'), convert(0, 'num256'), convert(0, 'num256'), convert(0, 'num256'), convert(0, 'num256')]
    if self.is_overflow_add(self.balances[_sender], _value):
        return _fail
    if self.is_overflow_add(self.num_issued, _value):
        return _fail

    self.balances[_sender] = num256_add(self.balances[_sender], _value)
    self.num_issued = num256_add(self.num_issued, _value)

    # return [throw_status, Log3("Transfer", 0, msg.sender, msg.value)]
    return [convert(1, 'num256'), convert(3, 'num256'), convert(keccak256("Transfer(address,address,uint256)"), 'num256'), convert(0, 'num256'), convert(convert(_sender, 'bytes32'), 'num256'), _msg_value]

@public
def withdraw(_sender : address, _msg_value : num256, _value : num256) -> num256[10]:
    _fail:num256[10] = [convert(0, 'num256'), convert(0, 'num256'), convert(0, 'num256'), convert(0, 'num256'), convert(0, 'num256'), convert(0, 'num256'), convert(0, 'num256'), convert(0, 'num256'), convert(0, 'num256'), convert(0, 'num256')]

    if _msg_value != convert(0, 'num256'):
        return _fail

    if self.is_overflow_sub(self.balances[_sender], _value):
        return _fail
    if self.is_overflow_sub(self.num_issued, _value):
        return _fail

    self.balances[_sender] = num256_sub(self.balances[_sender], _value)
    self.num_issued = num256_sub(self.num_issued, _value)

    # return [throw_status, ret_val, SEND(sender, value), Log3("Transfer", msg.sender, 0, value)]
    return [convert(1, 'num256'), convert(1, 'num256'), convert(5, 'num256'), convert(convert(_sender, 'bytes32'), 'num256'), _value, convert(3, 'num256'), convert(keccak256("Transfer(address,address,uint256)"), 'num256'), convert(convert(_sender, 'bytes32'), 'num256'), convert(0, 'num256'), _value]

@public
@constant
def totalSupply(_sender : address, _msg_value : num256) -> num256[2]:
    if _msg_value != convert(0, 'num256'):
        return [convert(0, 'num256'), convert(0, 'num256')]

    return [convert(1, 'num256'), self.num_issued]

@public
@constant
def balanceOf(_sender : address, _msg_value : num256, _owner : address) -> num256[2]:
    if _msg_value != convert(0, 'num256'):
        return [convert(0, 'num256'), convert(0, 'num256')]

    return [convert(1, 'num256'), self.balances[_owner]]

@public
def transfer(_sender : address, _msg_value : num256, _to : address, _value : num256) -> num256[7]:
    _fail:num256[7] = [convert(0, 'num256'), convert(0, 'num256'), convert(0, 'num256'), convert(0, 'num256'), convert(0, 'num256'), convert(0, 'num256'), convert(0, 'num256')]

    if _msg_value != convert(0, 'num256'):
        return _fail

    if self.is_overflow_add(self.balances[_to], _value):
        return _fail
    if self.is_overflow_sub(self.balances[_sender], _value):
        return _fail

    self.balances[_sender] = num256_sub(self.balances[_sender], _value)
    self.balances[_to] = num256_add(self.balances[_to], _value)

    # return [throw_status, ret_val, Log3("Transfer", msg.sender, _to, _value]
    return [convert(1, 'num256'), convert(1, 'num256'), convert(3, 'num256'), convert(keccak256("Transfer(address,address,uint256)"), 'num256'), convert(convert(_sender, 'bytes32'), 'num256'), convert(convert(_to, 'bytes32'), 'num256'), _value]

@public
def transferFrom(_sender : address, _msg_value : num256, _from : address, _to : address, _value : num256) -> num256[7]:
    _fail:num256[7] = [convert(0, 'num256'), convert(0, 'num256'), convert(0, 'num256'), convert(0, 'num256'), convert(0, 'num256'), convert(0, 'num256'), convert(0, 'num256')]

    if _msg_value != convert(0, 'num256'):
        return _fail

    allowance:num256 = self.allowances[_from][_sender]
    if self.is_overflow_add(self.balances[_to], _value):
        return _fail
    if self.is_overflow_sub(self.balances[_from], _value):
        return _fail
    if self.is_overflow_sub(allowance, _value):
        return _fail
    self.balances[_from] = num256_sub(self.balances[_from], _value)
    self.balances[_to] = num256_add(self.balances[_to], _value)
    self.allowances[_from][_sender] = num256_sub(allowance, _value)

    # return [throw_status, ret_val, Log3("Transfer", _from, _to, _value]
    return [convert(1, 'num256'), convert(1, 'num256'), convert(3, 'num256'), convert(keccak256("Transfer(address,address,uint256)"), 'num256'), convert(convert(_from, 'bytes32'), 'num256'), convert(convert(_to, 'bytes32'), 'num256'), _value]

@public
def approve(_sender : address, _msg_value : num256, _spender : address, _value : num256) -> num256[7]:
    _fail:num256[7] = [convert(0, 'num256'), convert(0, 'num256'), convert(0, 'num256'), convert(0, 'num256'), convert(0, 'num256'), convert(0, 'num256'), convert(0, 'num256')]

    if _msg_value != convert(0, 'num256'):
        return _fail

    self.allowances[_sender][_spender] = _value

    # return [throw_status, ret_val, Log3("Approval", msg.sender, _spender, _value)]
    return [convert(1, 'num256'), convert(1, 'num256'), convert(3, 'num256'), convert(keccak256("Approval(address,address,uint256)"), 'num256'), convert(convert(_sender, 'bytes32'), 'num256'), convert(convert(_spender, 'bytes32'), 'num256'), _value]

@public
@constant
def allowance(_sender : address, _msg_value : num256, _owner : address, _spender : address) -> num256[2]:
    if _msg_value != convert(0, 'num256'):
        return [convert(0, 'num256'), convert(0, 'num256')]

    return [convert(1, 'num256'), self.allowances[_owner][_spender]]
