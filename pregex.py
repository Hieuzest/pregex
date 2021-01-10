try:
    import regex as re
except ImportError:
    import re

import enum

from functools import lru_cache
import typing
from typing import Any, Callable, Dict, List, Tuple


class RegexFlag(enum.IntFlag):
    # Allow white characters between items
    SPLIT: int = 2**21


def escape_regex(s: str):
    return re.escape(s)


def escape(s: str, *, escape_comma: bool = True) -> str:
    s = s.replace("&", "&amp;") \
        .replace("[", "&#91;") \
        .replace("]", "&#93;")
    if escape_comma:
        s = s.replace(",", "&#44;")
    return s


def unescape(s: str) -> str:
    return s.replace("&#44;", ",") \
        .replace("&#91;", "[") \
        .replace("&#93;", "]") \
        .replace("&amp;", "&")


class _RepeatState:

    def __init__(self) -> None:
        super().__init__()
        self._n = 0
        self._map: Dict[str, List[str]] = {}
        self._map[None] = []
        self._stack: List[str] = []
        self._patterns = {}

    @property
    def current(self):
        return self._stack[-1] if self._stack else None

    def __enter__(self):
        self._n += 1
        self._stack.append(f"__repeat_{self._n}__")
        self._map[self.current] = []
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        self._stack.pop()

    def add(self, name):
        self._map[self.current].append(name)

    def check(self, pattern):
        self._patterns[self.current] = pattern
        return bool(self._map[self.current])

    def generate(self):
        res = {}
        for k, v in self._map.items():
            if not k or k in self._stack:
                continue
            for name in v:
                res[name] = (k, self._patterns[k])
        return res


class _StructState:

    def __init__(self) -> None:
        super().__init__()
        self._n = 0

    def __enter__(self):
        self._n += 1
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        self._n -= 1

    def check(self):
        return bool(self._n)


class _RequireState(dict):
    pass


_REQUIRE_DEFAULT_KEY = "__default_require_post__"


class _State:
    def __init__(self) -> None:
        super().__init__()
        self.repeat = _RepeatState()
        self.struct = _StructState()
        self.require = _RequireState()


class AbsPattern:

    @property
    @lru_cache()
    def regex(self) -> str:
        return str(compile(self))

    def _compile(self, flags: RegexFlag, *, state: _State) -> "Raw":
        raise NotImplementedError

    def __str__(self) -> str:
        return self.regex


class Pattern(AbsPattern):
    """
    ["a", "b"] -> "(ab)"
    ("a", "b") -> "(a|b)"

    """
    def __init__(self, pattern) -> None:
        super().__init__()
        self._pattern = pattern

    def _compile(self, flags: RegexFlag, *, state: _State) -> "Raw":
        return _compile(self._pattern, flags=flags, state=state)

    def __repr__(self) -> str:
        return f"{type(self).__name__}({self._pattern!r})"


class Raw(AbsPattern):
    def __init__(self, pattern: str) -> None:
        super().__init__()
        self._pattern = pattern

    def _compile(self, flags: RegexFlag, *, state: _State) -> "Raw":
        return self

    def __str__(self) -> str:
        return self._pattern

    def __repr__(self) -> str:
        return f"{type(self).__name__}({self._pattern!r})"


AnyPlain = Raw(r"[^,\[\]]*")
AnyElement = Raw(r"[^,\[\]]+|(\[[^\]]*\])")
AnyMessage = Raw(r"([^,\[\]]|(\[[^\]]*\]))*")
AnyBlank = Raw(r"\s*")


class CompiledPattern(Raw):
    def __init__(self, pattern, **kwargs) -> None:
        super().__init__(str(pattern))
        self._kwargs = kwargs

    def match(self, s: str, flags: RegexFlag = 0) -> "Match":
        m = re.match(self._pattern, s, flags=flags)
        return Match(match=m, flags=flags, **self._kwargs) if m else None


class Struct(AbsPattern):
    def __init__(self, type_, **kwargs):
        self._type = type_
        self._kwargs = kwargs

    def _compile(self, flags: RegexFlag, *, state: _State) -> "Raw":
        with state.struct:
            r = fr"(\[{_compile(self._type, flags=flags, state=state)}(?:,[a-zA-Z0-9-_.]+=[^,\]]+)*"
            kwargs = sorted(self._kwargs.items())
            for key, value in kwargs:
                r += fr",{_compile(key, flags=flags, state=state)}={_compile(value, flags=flags, state=state)}" r"(?:,[a-zA-Z0-9-_.]+=[^,\]]+)*"
            r += r",?\])"
        return Raw(r)

    def __repr__(self) -> str:
        return f"{type(self).__name__}({self._type!r}, **{self._kwargs!r})"


class Require(AbsPattern):
    def __init__(self, pattern=AnyPlain, *, name: str, post=...) -> None:
        """
            post: ... / Callable / (Union[Callable, Tuple[Callable]], Union[Callable, Tuple[Callable]])
        """
        self._pattern = pattern
        self._name = name
        self._post = post

    def _construct(self, p: Raw):
        return Raw(fr"(?P<{self._name}>{p})")

    def _compile(self, flags: RegexFlag, *, state:_State) -> "Raw":
        state.repeat.add(self._name)
        if self._post is ...:
            self._post = state.require.get(_REQUIRE_DEFAULT_KEY)
        if isinstance(self._post, Tuple):
            if isinstance(self._post[state.struct.check()], Tuple):
                state.require[self._name] = (unescape, *self._post[state.struct.check()]) if state.struct.check() else self._post[state.struct.check()]
            else:
                state.require[self._name] = (unescape, self._post[state.struct.check()]) if state.struct.check() else self._post[state.struct.check()]
        else:
            state.require[self._name] = (unescape, self._post) if state.struct.check() else self._post
        return self._construct(_compile(self._pattern, flags=flags, state=state))

    def __repr__(self) -> str:
        return f"{type(self).__name__}({self._pattern!r}, name={self._name!r})"


class Repeat(AbsPattern):

    Kleene = -1
    Optional = -2
    Plus = -3

    def __init__(self, pattern, times: int = -1) -> None:
        super().__init__()
        if isinstance(pattern, list) and not isinstance(pattern, str):
            self._pattern = pattern
        else:
            self._pattern = [pattern]
        self._times = times

    def _construct(self, p: Raw):
        if self._times == self.Kleene:
            return Raw(f"{p}*")
        elif self._times == self.Optional:
            return Raw(f"{p}?")
        elif self._times == self.Plus:
            return Raw(f"{p}+")
        else:
            return Raw(f"{p}{{{self._times}}}")

    def _compile(self, flags: RegexFlag, *, state: _State) -> "Raw":
        with state.repeat as repeat:
            p = _compile(self._pattern, flags=flags, state=state, raw=False)
            if repeat.check(p):
                p = self._construct(p)
                p = Require(p, name=repeat.current)
            else:
                p = self._construct(p)
        return _compile(p, flags=flags, state=state)
        
    def __repr__(self) -> str:
        if type(self) == Repeat:
            return f"{type(self).__name__}({self._pattern!r}, times={self._times})"
        else:
            return f"{type(self).__name__}({self._pattern!r})"


class Kleene(Repeat):
    def __init__(self, pattern) -> None:
        super().__init__(pattern, times=self.Kleene)


class Optional(Repeat):
    def __init__(self, pattern) -> None:
        super().__init__(pattern, times=self.Optional)


class Plus(Repeat):
    def __init__(self, pattern) -> None:
        super().__init__(pattern, times=self.Plus)


class Match:
    def __init__(self, match, flags, _repeat_map, _require_post) -> None:
        super().__init__()
        self._match: typing.Match = match
        self._flags: RegexFlag = flags
        self._repeat_map: Dict[str, Tuple[str, CompiledPattern]] = _repeat_map
        self._require_post: Dict[str, Callable] = _require_post
    
    @property
    def match(self) -> typing.Match:
        return self._match

    @property
    def string(self) -> str:
        return self._match.string

    @lru_cache()
    def _group(self, key: str):
        if self._repeat_map.get(key):
            repeat_key, repeat_pattern = self._repeat_map.get(key)

            outer_groups = self._group(repeat_key)
            if isinstance(outer_groups, str):
                outer_groups = [outer_groups]

            res = []
            for outer_group in outer_groups:
                for m in re.finditer(repeat_pattern.regex, outer_group):
                    inner_match = repeat_pattern.match(outer_group[m.start():m.end()], flags=self._flags)
                    r = inner_match._group(key)
                    if isinstance(r, str):
                        res.append(r)
                    else:
                        res.extend(r)
            return res
        else:
            return self._match.group(key)

    @lru_cache()
    def group(self, key: str):
        post = self._require_post.get(key)
        if post and not isinstance(post, Tuple):
            post = (post, )
        
        def _post(x):
            for _p in post:
                if _p:
                    x = _p(x)
            return x

        res = self._group(key)
        if post:
            if isinstance(res, str):
                return _post(res)
            else:
                return list(_post(r) for r in res)
        else:
            return res

    def __str__(self) -> str:
        return f"<pregex.Match of {self._match}>"

    def __repr__(self) -> str:
        return self.__str__()


def _enclose(s: str):
    return f"({s})"


def _escape(s: str, code=True, comma=True):
    if code:
        return escape_regex(escape(s, escape_comma=comma))
    else:
        return escape_regex(s)


def _compile(pattern, flags: RegexFlag, *, state: _State, raw: bool = True) -> Raw:
    if isinstance(pattern, AbsPattern):
        p = pattern._compile(flags=flags, state=state)
    elif isinstance(pattern, str):
        p = Raw(_enclose(_escape(pattern)))
    elif isinstance(pattern, tuple):
        p = Raw(_enclose('|'.join(str(_compile(p, flags=flags, state=state)) for p in pattern)))
    elif isinstance(pattern, list):
        if flags & RegexFlag.SPLIT:
            p = Raw(_enclose(str(AnyBlank) + str(AnyBlank).join(str(_compile(p, flags=flags, state=state)) for p in pattern) + str(AnyBlank)))
        else:
            p = Raw(_enclose(''.join(str(_compile(p, flags=flags, state=state)) for p in pattern)))
    else:
        raise ValueError(f"{pattern!r} is not a valid pattern!")
    if not raw:
        p = CompiledPattern(p, 
            _repeat_map=state.repeat.generate(), 
            _require_post=state.require,
        )
    return p


def compile(pattern, flags: RegexFlag = 0, *, default_require_post=...) -> CompiledPattern:
    state = _State()
    if default_require_post is not ...:
        if not isinstance(default_require_post, Tuple):
            default_require_post = (default_require_post, default_require_post)
        state.require[_REQUIRE_DEFAULT_KEY] = default_require_post

    return _compile(pattern, flags=flags, state=state, raw=False)


def match(pattern, message, flags: RegexFlag = 0, **kwargs):
    if not isinstance(message, str):
        message = str(message)
    if not isinstance(pattern, CompiledPattern):
        pattern = compile(pattern, flags=flags, **kwargs)
    return pattern.match(message, flags=flags)


def match_rawstring(pattern, string: str, flags: RegexFlag = 0):
    if not isinstance(pattern, CompiledPattern):
        pattern = compile(pattern, flags=flags, default_require_post=unescape)
    return pattern.match(escape(string), flags=flags)


__all__ = [
    'RegexFlag', 'escape', 'unescape', 'compile', 'match', 'match_rawstring', 
    'Pattern', 'Raw', 'Repeat', 'Kleene', 'Optional', 'Plus', 'Struct', 'Require', 
    'AnyPlain', 'AnyElement', 'AnyMessage', 'AnyBlank',
]
