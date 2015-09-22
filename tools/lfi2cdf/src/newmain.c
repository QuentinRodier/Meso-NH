#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <getopt.h>

#define BUFSIZE 4096

extern lfi2cdfmain_(char*, int*, int *, char*, int*, char*, int*, int*, int*, int*, int*, int*, int*, int*, int*, int*);

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
  int reduceprecision_flag;
  int outname_flag;
  int compress_flag, compress_level;
  int split_flag;
  int help_flag;
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

  compress_flag = 0;
  list_flag = 0;
  hdf5_flag = 0;
  help_flag = 0;
  outname_flag = 0;
  reduceprecision_flag = 0;
  split_flag = 0;
  p = buff;
  *p = '\0';

  /* Default values for merging of LFI splitted files */
  merge_flag = 0;
  nb_levels = 1;

  while (1) {
    int option_index = 0;

    static struct option long_options[] = {
      {"cdf4",             no_argument,       0, '4' },
      {"compress",         required_argument, 0, 'c' },
      {"help",             no_argument,       0, 'h' },
      {"list",             no_argument,       0, 'l' },
      {"merge",            required_argument, 0, 'm' },
      {"output",           required_argument, 0, 'o' },
      {"reduce-precision", no_argument,       0, 'r' },
      {"split",            no_argument,       0, 's' },
      {"var",              required_argument, 0, 'v' },
      {0,                  0,                 0,  0  }
    };

    c = getopt_long(argc, argv, "4c:hlm:o:rsv:",
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
    case 'c':
      compress_flag = 1;
      compress_level = atoi(optarg);
      if(compress_level<1 || compress_level>9) {
        printf("Error: compression level should in the 1 to 9 interval\n");
        exit(EXIT_FAILURE);
      }
      break;
    case '4':
      hdf5_flag = 1;
      break;
    case 'h':
      help_flag = 1;
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
    case 'r':
      reduceprecision_flag = 1;
      break;
    case 's':
      split_flag = 1;
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

  if (optind == argc || help_flag) {
    printf("usage : lfi2cdf [-h --help] [--cdf4 -4] [-l] [-v --var var1[,...]] [-r --reduce-precision] [-m --merge number_of_z_levels] [-s --split] [-o --output output-file.nc] [-c --compress compression_level] input-file.lfi\n");
    printf("        cdf2lfi [-o --output output-file.lfi] input-file.nc\n");
    printf("Usage: lfi2cdf [OPTION] ... lfi_file\n");
    printf("       cdf2lfi [OPTION] ... nc_file\n");
    printf("\nOptions:\n");
    printf("  --cdf4, -4\n");
    printf("     Write netCDF file in netCDF-4 format (HDF5 compatible) (lfi2cdf only)\n");
    printf("  --compress, -c compression_level\n");
    printf("     Compress data. The compression level should be in the 1 to 9 interval.\n");
    printf("     Only supported with the netCDF-4 format (lfi2cdf only)\n");
    printf("  --help, -h\n");
    printf("     Print this text\n");
    printf("  --list, -l\n");
    printf("     List all the fields of the LFI file and returns (lfi2cdf only)\n");
    printf("  --merge, -m number_of_z_levels\n");
    printf("     Merge LFI files which are split by vertical level (lfi2cdf only)\n");
    printf("  --output, -o\n");
    printf("     Name of file for the output\n");
    printf("  --reduce-precision, -r\n");
    printf("     Reduce the precision of the floating point variables to single precision (lfi2cdf only)\n");
    printf("  --split, -s\n");
    printf("     Split variables specified with the -v option (one per file) (lfi2cdf only)\n");
    printf("  --var, -v var1[,...]\n");
    printf("     List of the variable to write in the output file. Variables names have to be separated by commas (,).\n");
    printf("     A variable can be computed from the sum of existing variables (format: new_var=var1+var2[+...])\n");
    printf("     (lfi2cdf only)\n");
    printf("\n");
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

  /* Compression flag only supported if using netCDF4 */
  if (hdf5_flag==0 && compress_flag==1) {
	  compress_flag = 0;
	  printf("Warning: compression is forced to disable (only supported from netCDF4).\n");
  }

  /*
  printf("cmd=%s; inputfile=%s(%d); outputfile=%s(%d); varlistclean=%s with size : %d\n", cmd, 
         infile, ilen, outfile, olen, varlist, varlistlen);
  */

  /* Split flag only supported if -v is set */
  if (varlistlen==0) {
	  split_flag = 0;
	  printf("Warning: split option is forced to disable.\n");
  }


  lfi2cdfmain_(infile, &ilen, &outname_flag, outfile, &olen, varlist, &varlistlen, &l2c_flag, &list_flag, &hdf5_flag, &merge_flag,
		       &nb_levels, &reduceprecision_flag, &split_flag, &compress_flag, &compress_level);

  exit(EXIT_SUCCESS);
}
