# pregex

## Usage

```python
from pregex import *
pattern = Pattern(["The code is:", Kleene([Require(Raw("[0-9]+"), name="code", post=int), Optional(",")])])
message = "The code is: 123456, 3456"
res = match_rawstring(pattern, message, RegexFlag.SPLIT)
res.group("code")
```
