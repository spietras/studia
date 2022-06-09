//sizes in bytes

#define BYTE_SIZE                   1
#define WORD_SIZE                   2
#define DWORD_SIZE                  4
#define QWORD_SIZE                  8

#define SYMBOLS 256
#define SIZE_OF_FILE_LENGTH         QWORD_SIZE
#define SIZE_OF_SYMBOL              BYTE_SIZE
#define SIZE_OF_SYMBOLS_COUNT       WORD_SIZE
#define SIZE_OF_SYMBOL_FREQUENCY    QWORD_SIZE

#define MAXHEADERSIZE               (SIZE_OF_FILE_LENGTH + SIZE_OF_SYMBOLS_COUNT + (SIZE_OF_SYMBOL + SIZE_OF_SYMBOL_FREQUENCY)*SYMBOLS)