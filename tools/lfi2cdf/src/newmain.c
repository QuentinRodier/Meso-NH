#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <getopt.h>

#define BUFSIZE 4096

extern lfi2cdfmain_(char*, int*, int *, char*, int*, char*, int*, int*, int*, int*, int*, int*);

char *cleancomma(char *varlist)
{
  char *ip, *op;

  op = varlist;
  
  for (ip=varlist; *ip; ip++) {
    if (*ip != ',' || *ip == ',' && *op != ',') 
      *(++op) = *ip;
  }
  if (*op != ',') 
    *(++op) = ',';

  *(op+1) = '\0';
  return varlist+1;
}

int main(int argc, char **argv)
{
  int ilen;
  int list_flag;
  int l2c_flag;
  int hdf5_flag;
  int merge_flag, nb_levels;
  int outname_flag;
  char *cmd, *infile;
  int c;
  char buff[BUFSIZE];
  int varlistlen;
  char *varlist;
  char *p;
  int lenopt;
  char *outfile=NULL;
  int olen=0;

  cmd = strrchr(argv[0], '/');
  if (cmd == NULL)
    cmd = argv[0];
  else
    cmd++;
  l2c_flag = strcmp(cmd, "lfi2cdf") == 0 ? 1 : 0;

  list_flag = 0;
  hdf5_flag = 0;
  p = buff;
  *p = '\0';

  /* Default values for merging of LFI splitted files */
  merge_flag = 0;
  nb_levels = 1;

  while (1) {
    int option_index = 0;

    static struct option long_options[] = {
      {"cdf4",             no_argument,       0, '4' },
      {"list",             no_argument,       0, 'l' },
      {"merge",            required_argument, 0, 'm' },
      {"output",           required_argument, 0, 'o' },
      {"var",              required_argument, 0, 'v' },
      {0,                  0,                 0,  0  }
    };

    c = getopt_long(argc, argv, "4lm:o:v:",
		    long_options, &option_index);
    if (c == -1)
      break;

    switch (c) {
    case 0:
      printf("option %s", long_options[option_index].name);
      if (optarg)
	printf(" with arg %s", optarg);
      printf("\n");
      break;
    case '4':
      hdf5_flag = 1;
      break;
    case 'l':
      list_flag = 1;
      break;
    case 'm':
      merge_flag = 1;
      nb_levels = atoi(optarg);
      break;
    case 'o':
      outname_flag = 1;
      outfile = optarg;
      olen = strlen(outfile);
      break;
    case 'v':
      if (l2c_flag) {
	lenopt = strlen(optarg);
	//	printf("option v with value '%s'\n", optarg);
	if (p+lenopt > buff+BUFSIZE)
	  printf("%s ignored in list\n", optarg);
	else {
	  *p++ = ',';
	  strcpy(p, optarg);
	  p += lenopt;
	}
      } else 
	printf("option -v is ignored\n"); 
      break;

    default:
      printf("?? getopt returned character code 0%o ??\n", c);
    }
  }

  if (optind == argc) {
    printf("usage : lfi2cdf [--cdf4 -4] [-l] [-v --var var1[,...]] [-m --merge number_of_z_levels] [-o --output output-file.nc] input-file.lfi\n");
    printf("        cdf2lfi [-o --output output-file.lfi] input-file.nc\n");
    exit(EXIT_FAILURE);
  } 

  ilen = strlen(argv[optind]);
  infile = argv[optind];

  varlist = cleancomma(buff);
  varlistlen = strlen(buff);
  
  if (outfile == NULL) {
    /* determine outfile name from infile name */
    char *cp, *sp;
    cp = strrchr(infile, '/');
    if (cp == 0)                /* no delimiter */
      cp = infile;
    else                        /* skip delimeter */
      cp++;
    outfile = (char*) malloc((unsigned)(strlen(cp)+5));
    (void) strncpy(outfile, cp, strlen(cp) + 1);
    if ((sp = strrchr(outfile, '.')) != NULL)
      *sp = '\0';
    if (l2c_flag){
      char *ncext;
      ncext = hdf5_flag ? ".nc4" : ".nc"; 
      strcat(outfile,ncext);
    } else
      strcat(outfile,".lfi");
    olen = strlen(outfile);
  }

  /*
  printf("cmd=%s; inputfile=%s(%d); outputfile=%s(%d); varlistclean=%s with size : %d\n", cmd, 
         infile, ilen, outfile, olen, varlist, varlistlen);
  */

  lfi2cdfmain_(infile, &ilen, &outname_flag, outfile, &olen, varlist, &varlistlen, &l2c_flag, &list_flag, &hdf5_flag, &merge_flag,
		       &nb_levels);

  exit(EXIT_SUCCESS);
}
