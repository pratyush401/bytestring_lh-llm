v1
- 10 modules (up to Data/ByteString/Internal/Pure.hs)
- 104 functions

v2
- 10 modules (up to Data/ByteString/Internal/Pure.hs)
- 135 functions

v3
- 16 modules (up to Data/ByteString/Unsafe.hs)
- 361 unique functions (by name)
- 393 total functions (including same name, different files)

latest (2024-06-04)
- 16 modules (up to Data/ByteString/Unsafe.hs)
- 378 unique functions
- 411 total functions
- added the following 17 functions:
    - 'c_memcmp_ByteArray'
    - 'contramap'
    - 'copy'
    - 'fillFrom'
    - "freeze'"
    - 'go'
    - "go'"
    - 'go2'
    - 'goWord64Chunk'
    - 'goWord8Chunk'
    - 'indexIsCont'
    - 'intoWord'
    - 'karpRabin'
    - 'loop'
    - 'pair'
    - 'shift'
    - 'v'
- switch bytestring_dependencies.json to (file, func) instead of (func, file)
