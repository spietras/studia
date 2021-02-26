#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "FileSystem.h"

void printError(const char *message, int error)
{
    printf("%s\n", message);
    switch (error)
    {
        case 1:
            printf("File modification error. Disk may be corrupted\n");
            break;
        case 2:
            printf("File position error. Disk may be corrupted\n");
            break;
        case 3:
            printf("Memory allocation error. Disk may be corrupted\n");
            break;
        case 4:
            printf("Not enough space\n");
            break;
        case 5:
            printf("Can't find file\n");
            break;
        default:
            printf("Something weird happened\n");
            break;
    }
}

int main(int argc, char *argv[])
{
    char *fileSystemName;
    char *command;

    if (argc <= 2)
    {
        printf("Usage: %s [file system name] -command [...]\n", argv[0]);
        printf(
                "Availible commands: \n"
                "create [size in bytes]\n"
                "copyin [source name] [destination name]\n"
                "copyout [source name] [destination name]\n"
                "list\n"
                "remove [file name]\n"
                "delete\n"
                "bitmap\n"
        );
        return -1;
    }

    fileSystemName = argv[1];
    command = argv[2];
    int r = 0;

    if (strcmp("-create", command) == 0)
    {
        if (argc == 4)
        {
            sizeType size = (sizeType) atoi(argv[3]);
            FileSystem *fs = createFileSystem(fileSystemName, size);
            if (!fs)
            {
                printf("Can't create file system\n");
                return 1;
            }
            r = closeFileSystem(fs);
            if (r)
            {
                printError("Can't close file system", r);
                return r;
            }
        }
        else
        {
            printf("Usage: %s [file system name] -create [size in bytes]\n", argv[0]);
            return -1;
        }
    }
    else if (strcmp("-copyin", command) == 0)
    {
        if (argc == 5)
        {
            char *sourceFileName = argv[3], *destFileName = argv[4];
            FileSystem *fs = loadFileSystem(fileSystemName);
            if (!fs)
            {
                printf("Can't load file system\n");
                return 1;
            }
            r = copyFromOutside(fs, sourceFileName, destFileName);
            if (r)
            {
                printError("Can't copy in file", r);
                return r;
            }
            r = closeFileSystem(fs);
            if (r)
            {
                printError("Can't close file system", r);
                return r;
            }
        }
        else
        {
            printf("Usage: %s [file system name] -copyin [source name] [destination name]\n", argv[0]);
            return -1;
        }
    }
    else if (strcmp("-copyout", command) == 0)
    {
        if (argc == 5)
        {
            char *sourceFileName = argv[3], *destFileName = argv[4];
            FileSystem *fs = loadFileSystem(fileSystemName);
            if (!fs)
            {
                printf("Can't create file system\n");
                return 1;
            }
            r = copyToOutside(fs, sourceFileName, destFileName);
            if (r)
            {
                printError("Can't copy out file", r);
                return r;
            }
            r = closeFileSystem(fs);
            if (r)
            {
                printError("Can't close file system", r);
                return r;
            }
        }
        else
        {
            printf("Usage: %s [file system name] -copyout [source name] [destination name]\n", argv[0]);
            return -1;
        }
    }
    else if (strcmp("-list", command) == 0)
    {
        if (argc == 3)
        {
            FileSystem *fs = loadFileSystem(fileSystemName);
            if (!fs)
            {
                printf("Can't create file system\n");
                return 1;
            }
            displayFiles(fs);
            r = closeFileSystem(fs);
            if (r)
            {
                printError("Can't close file system", r);
                return r;
            }
        }
        else
        {
            printf("Usage: %s [file system name] -list\n", argv[0]);
            return -1;
        }
    }
    else if (strcmp("-remove", command) == 0)
    {
        if (argc == 4)
        {
            char *toRemove = argv[3];
            FileSystem *fs = loadFileSystem(fileSystemName);
            if (!fs)
            {
                printf("Can't create file system\n");
                return 1;
            }
            r = removeFile(fs, toRemove);
            if (r)
            {
                printError("Can't remove file", r);
                return r;
            }
            r = closeFileSystem(fs);
            if (r)
            {
                printError("Can't close file system", r);
                return r;
            }
        }
        else
        {
            printf("Usage: %s [file system name] -remove [file name]\n", argv[0]);
            return -1;
        }
    }
    else if (strcmp("-delete", command) == 0)
    {
        if (argc == 3)
        {
            r = deleteFileSystem(fileSystemName);
            if (r)
            {
                printError("Can't delete file system", r);
                return r;
            }
        }
        else
        {
            printf("Usage: %s [file system name] -delete\n", argv[0]);
            return -1;
        }
    }
    else if (strcmp("-bitmap", command) == 0)
    {
        if (argc == 3)
        {
            FileSystem *fs = loadFileSystem(fileSystemName);
            if (!fs)
            {
                printf("Can't create file system\n");
                return 1;
            }
            displayDataBitmap(fs);
            r = closeFileSystem(fs);
            if (r)
            {
                printError("Can't close file system", r);
                return r;
            }
        }
        else
        {
            printf("Usage: %s [file system name] -bitmap\n", argv[0]);
            return -1;
        }
    }
    else
    {
        printf("Invalid command: `%s`\n", command);
        return -1;
    }


    return 0;
}