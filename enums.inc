!! enumerated type/parameter values for use in IO offloading

integer, parameter :: IO_OP_OPEN = 1, IO_OP_REGISTER_AXIS = 2, IO_OP_REGISTER_FIELD = 3, IO_OP_WRITE = 4, &
                      IO_OP_CLOSE = 5, IO_OP_EXIT = 6

integer, parameter :: DOUBLE = 1, FLOAT = 2, INTEGER = 3, LOGICAL = 4, CHARACTER = 5