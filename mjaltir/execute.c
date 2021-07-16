/*
   Function to execute programs from inside fortran programs. This
   helps user programs to communicate with the operating system by
   spawning new child processes and executing programs available in the
   user's designated path.  Pipe is not supported, but background 
   job is supported.  When a process is run in background, parent and child
   processes run independently and in parallel, thus cannot support
   dependency between the parent and children.
 
   usage example:
 
      character *512 buffer
      open(5, file = 'xxxx', status = 'UNKNOWN')
      buffer = 'echo My name is Muthian George.'
      write(5, '(''I am working in the Division of Animal and'')')
      write(5, '(''Poultry Science, University of Guelph'')')
      close(5)
      call exec(buffer)
      call exec('cat xxxx')
      call exec('rm xxxx')
c     runs directory listing in background.
c     note that directory listing appears after the execution stops.      
      call exec('ls -ls &')
      stop
      end
 
   compile the above program "test.f" using the fortran compiler as:
 
   %ff -o test test.f execute.c
 
   when "test" is executed, the program spawns a new process and calls
   "echo" "cat" and "rm" with the above arguments and execute them.
 
   This module is very useful for fortran users to execute multiple programs
   within their own programs.  For example, files can be sorted or merged
   within user programs by calling "msort".
 
   Author of the Module: Muthian George.
*/
 
#include <stdio.h>
#include <sys/wait.h>
#include <ctype.h>
extern char **environ;
typedef int BOOL;
#define ARGC (64)
#define YES (1)
#define NO (0)
 
BOOL exec_(buffer, length)
int length;
char *buffer;
{
  int argc, flag, background, avlastlen, pid, status;
  char execname[BUFSIZ], *argv[ARGC], **en, *pgmname;
  register i;
  if((pgmname = (char *) malloc(length+1)) == NULL)
    {
      fprintf(stderr, "cannot allocate buffer\n");
      return NO;
    }
  memcpy(pgmname, buffer, length);
  pgmname[length] = '\0';
  en = environ;
  argc = flag = 0;
  for(i=0; i < length; ++i)
    if(isspace(pgmname[i]) && flag)
      {
	flag = NO;
	pgmname[i] = '\0';
      }
    else if( ! isspace(pgmname[i]))
      {
	if( ! flag)
	  {
	    if(argc < ARGC - 1)
	      argv[argc ++] = pgmname + i;
	    else
	      {
		fprintf(stderr, "argc exceeds %d arguments\n", ARGC);
		free(pgmname);
		return NO;
	      }
	    flag = YES;
	  }
      }
    else pgmname[i] = '\0';
  pgmname[i] = '\0';
  argv[argc] = NULL;
  avlastlen = strlen(argv[argc-1]);
  if(argv[argc-1][avlastlen-1] == '&')
    {
      background = YES;
      if(avlastlen == 1)
	  argv[-- argc] = NULL;
      else
	argv[argc-1][avlastlen-1] = '&';
    }
  else
    background = NO;
  strcpy(execname, argv[0]);
  if(background)
    {
      if((pid = fork()) == 0)
	execvp(execname, argv, en);
    }
  else
    {
      if((pid = fork()) != 0)
	{
	  if(wait(&status) == -1)
	    {
	      fprintf(stderr, "execution failed\n");
	      free(pgmname);	
	      return NO;
	    }
	}
      else
	execvp(execname, argv, en);
    }
  free(pgmname);
  return YES;
}
 
memcpy(buf1, buf2, size)
char *buf1, *buf2;
int size;
{
  while (--size >= 0) *buf1++ = *buf2++;
}
 
BOOL cd_(buffer, length)
int length;
char *buffer;
{
  char *pathname;
  if((pathname = (char *) malloc(length+1)) == NULL)
    {
      fprintf(stderr, "cannot allocate buffer\n");
      return NO;
    }
  memcpy(pathname, buffer, length);
  pathname[length] = '\0';
  if(chdir(pathname) != 0) 
    {
      fprintf(stderr,"wrong path: %s\n", pathname);
      free(pathname);
      return NO;
    }
  free(pathname);
  return YES;
}
 
BOOL strim_(length,name)
int length;
char *name;
{
  register i;
  for(i=length-1; i>=0; i--)
    if(! isspace(name[i])) break;
  name[i+1] = '\0';
  return 1;
}
 
BOOL scat_(length1,name1,length2,name2)
int length1,length2;
char *name1,*name2;
{
  strcat(name1,name2);
  return 1;
}
 
BOOL atoi_(len, var, ival)
int len, *ival; char *var;
{
  *ival = atoi(var);
  return 1;
}
 
BOOL atod_(len, var, ival)
int len; double *ival; char *var;
{
  *ival = atof(var);
  return 1;
}
 
BOOL itoa_(ival, len, var)
int len, *ival; char *var;
{
  sprintf(var, "%d", *ival);
  return 1;
}
 
BOOL dtoa_(ival, len, var)
int len; double *ival; char *var;
{
  sprintf(var, "%f", *ival);
  return 1;
}
