#ifndef VOLUME_OPERATIONS_H_INCLUDED
#define VOLUME_OPERATIONS_H_INCLUDED

#include "definitions.h"

int ResizeVolume(Volume*, const int);
Volume* CreateVolume(const char*, const int);
Volume* Load(const char*);
int Save(const Volume*, const char*);
Volume* InitializeVolume();

#endif