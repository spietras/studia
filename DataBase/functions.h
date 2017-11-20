#ifndef FUNCTIONS_H_INCLUDED
#define FUNCTIONS_H_INCLUDED

struct Club
{
	int index;
	char name[256];
	int league;
	unsigned long fansNumber;
	unsigned long stadiumCapacity;
	unsigned long budget;
	unsigned long leaguePosition;
};

int Initialize(struct Club **, int *, struct Club **, int *);
struct Club * Allocate(int n);
struct Club * Reallocate(struct Club *, int n);
void ShowClub(struct Club *);
void ShowAll(struct Club *, int);
int AddClub(struct Club **, int *);
int RemoveClub(struct Club **, int *);
struct Club * Search(struct Club *, int, struct Club **, int *);
struct Club * SearchByExact(struct Club *, int, struct Club **, int *, int);
struct Club * SearchByRange(struct Club *, int, struct Club **, int *, int);
struct Club * SearchByMore(struct Club *, int, struct Club **, int *, int);
struct Club * SearchByLess(struct Club *, int, struct Club **, int *, int);
unsigned long GetValue(struct Club *, int, int);
int ShowMenu(struct Club **, int *, struct Club **, int *);
unsigned long ToNumber(char *, int);
unsigned long GetNumberInput(char*, char*);
void GetStringInput(char *, char *, char *);
void WaitToContinue();
unsigned long TenPower(int);

#endif