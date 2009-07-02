/**********************************************************
  $Author: REMOVED
  $Date: 2007/01/29 13:53:32 $
  $Revision: 1.9 $

  *********************************************

  This process reads a TM or TC packet and splits it into "commands"
  for qlaclient. These "commands" are then written to an
  output file or sent to qlaclient's using sockets.

***********************************************************/

#define _GNU_SOURCE     //(before any #include)
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <time.h>
#include <string.h>
#include <signal.h>
#include <unistd.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/select.h>
#include <sys/wait.h>
#include <sys/timeb.h>
#include <sys/stat.h>
#include <netinet/in.h>
#include <netdb.h>
#include <arpa/inet.h>
#include <fcntl.h>
/* #include <publib.h> */

#include <search.h>     //(for hash table gnu style: hcreate_r, hsearch_r ...)

/* #include "HL1_publib/PlanckL1Logger.h" */

/* #include "HL1_tools/tools.h" */

/* #include "HL1_MIBlib/MIB_structs.h" */
/* #include "HL1_MIBlib/MIB_structsImpl.ph" */
/* #include "HL1_MIBlib/MIB_reader.ph" */

/* #include "HL1_PUSlib2/TMTCPacketStructs.h" */
/* #include "HL1_PUSlib2/TMTCPacketImpl.ph" */
/* #include "HL1_PUSlib2/TMTCPacket.ph" //note: needs to be included after MIBlib headers */

#include "tmunpack-1.9.h"
/* #include "HL1_TmUnpack/decompress.ph" */

extern int errno ;
extern char *optarg ;
extern int optind ;

#define MAX_LENGTH_LINE 16384 //Maximum number of characters allowed in a line for the output file or socket

//----------------------------------- Other variable declarations -----------------------------------------------
//Global because needed by signal interrupts
int NbItemsFound  = 0 ;
int NbSetsSent  = 0 ;
int InSocket;             //Incoming data socket
int OutSocket;             //Outgoing data socket
int log_level ;

enum {
        aFILE,
        aFILES,
        aSOCKET,
        aSIMUL
} ;
enum {
        aYES,
        aNO
} ;

//--------------------------------------- Process signal functions -----------------------------------------------
void sig_usr( int sig )
{
  sig = sig;
  fprintf( stderr, "%d items found\n", NbItemsFound ) ;
  fflush( stderr ) ;
  log_level--; //increases the messages written out
  if ( log_level < 0 )log_level = 0 ;
  signal( SIGUSR1, sig_usr ) ;
}

void sig_usr2( int sig )
{
  sig = sig;
  log_level = 4 ; //only fatal messages
  signal( SIGUSR2, sig_usr2 ) ;
}

void sig_tst_hand( int sig )
{
  sig = sig;
  //Ciao( "Got a QUIT or KILL signal" ) ;
  fprintf( stdout, "Stopped by \"Got a QUIT or KILL signal\"\n" ) ;
  fprintf( stdout, "%d items found\n", NbItemsFound ) ;
  //elogStop(op) ;need to pass op structure !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  exit( 0 ) ;
}

void sig_hand( int sig )
{
  sig = sig;
  int is_ok ;
  //printf( "sig_hand: %d\n", sig ) ; fflush( stdout ) ;
  is_ok = write( InSocket, "Bye", 3 ) ;
  close( InSocket ) ;
  //Ciao( "Got a Sigquit" ) ;
  fprintf( stdout, "Stopped by \"Got a Sigquit\"\n" ) ;
  fprintf( stdout, "%d items found\n", NbItemsFound ) ;
  //elogStop(op) ;need to pass op structure !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  exit( 0 ) ;
}
void sig_kill( int sig )
{
  sig = sig;
  exit(0);
}
void sig_broken_pipe( int s )
{
  s=s;
  // try to ignore it
  fprintf(stdout, "SIG PIPE caught\n");
  signal( SIGPIPE, sig_broken_pipe ) ;
}

void sig_reveil( int sig )
{
  sig=sig;
}
//End of Process signal functions

void getDate( time_t *ttt, char *date )
{
          struct tm ttm ;
            ttm = *localtime( ttt ) ;
              sprintf( date, "%02d%02d%02d%02d%02d",ttm.tm_mon + 1, ttm.tm_mday,
                                         ttm.tm_hour, ttm.tm_min, ttm.tm_sec ) ;
}

//--------------------------------- Command option functions --------------------------------------------------
void Help(OPTIONS_struct * op)
{
  puts( "tmunpack [<options>]" ) ;
  puts( "Options:" ) ;
  puts(op->Options);
  puts( "  -h <host>   : Input Host IP name or Address. Default is \"localhost\". Socket is default (for other, use options -i or -l)." ) ;
  puts( "  -p <port>   : Input TCP Port number. Default is \"44444\"." ) ;
  puts( "  -s <sec>    : Dont quit when socket closed. Wait <sec> before retry. Default is 1 sec." ) ;
  puts( "  -i <file>   : Read from file instead of socket. Give <full path name of input file>." ) ;
  puts( "  -j <file>   : Read from files instead of socket." ) ; 
  puts( "                Give <full path name of file containing list of files (full path names, one per line)>." ) ;
  puts( "  -H <host>   : Output Host IP name or Address. " ) ;
  puts( "                Default is \"localhost\". Socket is default (for other, use options -i or -l)." ) ;
  puts( "  -P <port>   : Output TCP Port number. Default is \"33333\"." ) ;
  puts( "  -I <file>   : Write to file instead of socket. Give <full path name of output file>." ) ;
  puts( "  -J <file>   : Write to files instead of socket. Give <full path of prefix name of output files>." ) ;
  puts( "  -O <dir>    : Directory where files are written (give <existing directory>, default is current) " ) ;
  puts( "                or PIOLIB database created (give <existing directory>/<database name> ");
  puts( "                (default is <current path>/db_<date>)." ) ;
  puts( "  -v <num>    : Minimum log level. Give number 0-4: 0 for all (chat, note, warn, error, fatal), ... , ");
  puts( "                4 for fatal only. Default is 3 (error and fatal messages only)." ) ;
  puts( "  -L <path>   : Path of the log file. Default is \"stdout\"." ) ;
  puts( "  -E <host>   : Send message to elog. Give <hostname of the elog server>.");
  puts( "  -e          : Send message to elog. Default server used is \"plck-ccas-3.ias.u-psud.fr\".");
  puts( "  -a <date>   : Date min for run in year month day. Default is \"20050101\"." ) ;
  puts( "  -b <date>   : Date max for run in year month day. Default is \"20081231\"." ) ;
  //not needed, done by PUSlib:
  //puts( "  -k <path>   : Path of file containing the list of packets to be treated.\n") ;
  puts( "  -u <dpu>    : DPU version (v3r3p2 or v3r4 or v3r5 (default v3r5)"  ) ;
  puts( "                where v3r3p2 is before science packet modification"  ) ;
  puts( "                where v3r4   is after  science packet modification (April 7,2006) "  ) ;
  puts( "                where v3r5   is calibration PFM version"  ) ;
  puts( "  -S          : For producing simulated data"  ) ;
  puts( "  -d <val>    : If not set, the sampling divisor is extracted from HSK4-9." ) ;
  puts( "                If set, the sampling divisor is set to value <val>"  ) ;
  puts( "  -m <path>   : Path of directory containing MIB '.dat' files. Default is \"/home/instal/planck/HL1/HPATP2/TESTENV/data/ASCII\"." ) ;
  puts( "  -?          : Usage help.") ;
  exit( 1 ) ;
}

void HandleOptions(  int argc, char **argv , OPTIONS_struct * op)
{
  int opt ;
  //strcpy(op->Options, "h:p:s:i:j:H:P:I:J:O:v:L:E:a:b:k:u:d:m:Se?") ;
  strcpy(op->Options, "h:p:s:i:j:H:P:I:J:O:v:L:E:a:b:u:d:m:Se?") ;
  op->StartTime = time( NULL ) ;
  op->LogFile = stdout ;
  op->HostName = getenv( "HOSTNAME" ) ;
  op->UserName = getenv( "USER" ) ;

  //Default values
  op->InHost = NULL ;
  op->OutHost = NULL ;
  op->InPort = 44444 ;
  op->OutPort = 33333 ;
  op->InSocket = 0;
  InSocket = 0;
  OutSocket = 0;
  op->OutSocket = 0;
  op->RetryWait = 1 ;
  op->option_input              = aSOCKET ;     //input data from file or socket
  op->option_output              = aSOCKET ;     //input data from file or socket
  op->InFileName = NULL ;
  op->InFileNameList = NULL ;
  op->OutFileName = NULL ;
  op->OutFile = NULL ;
  op->OutputDir = NULL ;
  log_level = 3 ;
  op->log_level = 3 ;
  op->LogName = NULL ;
  op->LogFile = NULL ;
  op->option_send_elog  = aNO ; //send messages to Elog server
  op->ElogServer = NULL ;
  op->ElogMsg = NULL ;
  op->ElogCmd[0]='\0';
  op->ElogMsgFile[0]='\0';
  op->elogFormat[0]='\0';
  op->MaxDate=20081231;
  op->MinDate=20050101;
  op->UserName=NULL;
  op->HostName=NULL ;
  op->StartTime=0;
  op->EndTime=0 ;
  op->NbItemsFound=0 ;
  NbItemsFound=0 ;
  op->DPU_version=35;
  op->SamplingDivisor=0.0;
  //op->PacketToTreatFileName=NULL ;
  op->MIBPath = NULL ;

//TODO: socket related parameters, those that are commented are initialised above
  //input socket parameters:
  //op->option_input = aSOCKET (default value, that is when -i and -j not given). Used to determine what comes in. 
  //op->InHost : Input Host IP name or Address (SERVER_NAME?)
  //op->InPort : Input TCP Port number (SERVER_PORT?)
  //op->RetryWait : Don't quit when socket closed, wait <sec> before retry (default is 1 sec) (RECONNECT_PERIOD_S?)
  op->input_client_buff=1024; //(BUFF_SIZE)
  op->input_read_timeout_ms=200;

  //output socket parameters:
  //op->option_output = aSOCKET (default value, that is when -I and -J not given). Used to determine what goes out. 
  //op->OutHost : Output Host IP name or Address 
  //op->OutPort : Output TCP Port number (PORT?)
  op->output_server_buffer_size=1024;
  op->output_each_client_buffer_size=2048;
  op->output_send_timeout_ms=200;
  op->output_maximum_clients_number=2;
  op->output_do_not_lose_data=1;

  char date[32] ;
  time_t ttt = time( NULL ) ;
  getDate( &ttt, date ) ;

  while ( (opt = getopt( argc, argv, op->Options )) != EOF )
    switch( opt ) {
    case 'h': op->InHost = malloc( strlen( optarg ) + 1 ) ;
      strcpy( op->InHost, optarg ) ;
      break ;
    case 'p': sscanf( optarg, "%d", &(op->InPort) ) ;
      break ;
    case 's': sscanf( optarg, "%d", &(op->RetryWait) ) ;
      break ;
    case 'i': op->option_input = aFILE ;
      op->InFileName = malloc( strlen( optarg ) + 1 ) ;
      strcpy( op->InFileName, optarg ) ;
      break ;
    case 'j': op->option_input = aFILES ;
      op->InFileNameList = malloc( strlen( optarg ) + 1 ) ;
      strcpy( op->InFileNameList, optarg ) ;
      break ;
    case 'H': op->OutHost = malloc( strlen( optarg ) + 1 ) ;
      strcpy( op->OutHost, optarg ) ;
      break ;
    case 'P': sscanf( optarg, "%d", &(op->OutPort) ) ;
      break ;
    case 'I': op->option_output = aFILE ;
      op->OutFileName = malloc( strlen( optarg ) + 1 ) ;
      strcpy( op->OutFileName, optarg ) ;
      break ;
    case 'J': op->option_output = aFILES ;
      op->OutFileName = malloc( strlen( optarg ) + 1 ) ;
      strcpy( op->OutFileName, optarg ) ;
      break ;
    case 'O': op->OutputDir = malloc( strlen( optarg ) + 1 ) ;
      strcpy( op->OutputDir, optarg ) ;
      break ;
    case 'v': sscanf( optarg, "%d", &op->log_level ) ;
      log_level=op->log_level;
      break ;
    case 'L': op->LogName = malloc( strlen( optarg ) + strlen ( date ) + 2 ) ;
      sprintf( op->LogName, "%s_%s", optarg, date ) ;
      break ;
    case 'E': op->option_send_elog = aYES ;
      op->ElogServer = malloc( strlen( optarg ) + 1 ) ;
      strcpy( op->ElogServer, optarg ) ;
      break;
    case 'a': op->MinDate = atoi( optarg );
      break ;
    case 'b': op->MaxDate = atoi( optarg );
      break ;
    //case 'k': op->PacketToTreatFileName = malloc( strlen( optarg ) + 1 ) ;
    //  strcpy( op->PacketToTreatFileName, optarg ) ;
    //  break ;
    case 'u': 
      if( !strcmp(optarg,"v3r3p2") ) op->DPU_version = 332;
      if( !strcmp(optarg,"v3r4"  ) ) op->DPU_version = 34;
      if( !strcmp(optarg,"v3r5"  ) ) op->DPU_version = 35;
      break ;
    case 'd': sscanf( optarg, "%lf", &op->SamplingDivisor ) ;
      break ;
    case 'S': op->option_input  = aSIMUL ;
      break ;
    case 'e': op->option_send_elog = aYES ;
      break;
    case 'm': op->MIBPath = malloc( strlen( optarg ) + 1 ) ;
      strcpy( op->MIBPath, optarg ) ;
      break ;
    default: Help(op) ;
    }


  //Default values
  char * default_host = "localhost";
  char * current_dir = get_current_dir_name ();
  char * default_log = "stdout";
  char * default_elog = "plck-ccas-3.ias.u-psud.fr";
  char * default_mib_path = "/home/instal/planck/HL1/HPATP2/TESTENV/data/ASCII";
  if(op->InHost == NULL){
    op->InHost = malloc( strlen( default_host ) + 1 ) ;
    strcpy( op->InHost, default_host ) ;
  }
  if(op->OutHost == NULL){
    op->OutHost = malloc( strlen( default_host ) + 1 ) ;
    strcpy( op->OutHost, default_host ) ;
  }
  if(op->OutputDir == NULL){
    op->OutputDir = malloc( strlen( current_dir ) + strlen ( date ) + 2 ) ;
    sprintf( op->OutputDir, "%s_%s", current_dir, date ) ;
  }
  if(op->LogName == NULL){
    op->LogName = malloc( strlen( default_log ) + 1 ) ;
    strcpy( op->LogName, default_log ) ;
  }
  if(op->ElogServer == NULL){
    op->ElogServer = malloc( strlen( default_elog ) + 1 ) ;
    strcpy( op->ElogServer, default_elog ) ;
  }
  if(op->MIBPath == NULL){
    op->MIBPath = malloc( strlen( default_mib_path ) + 1 ) ;
    strcpy( op->MIBPath, default_mib_path ) ;
  }
//TODO: Control command options
}

int GenerateSimulatedData (char *ObjectBuffer){
  int i;
  unsigned long len ;
  char temp[100];
  double val;
  unsigned long maxnb=500;
  unsigned long maxgrp=10;
  unsigned long randnb;
  unsigned long randgrp,randobj;
  //Attention : ObjectBuffer is a: char ObjectBuffer[20000]
  maxnb=500ul;
  maxgrp=10ul;

  randnb= (unsigned long)(((double)maxnb -1.0)*(double)rand()/((double)RAND_MAX + 1.0));
  randgrp=(unsigned long)(((double)maxgrp-1.0)*(double)rand()/((double)RAND_MAX + 1.0));
  randobj=(unsigned long)(((double)maxgrp-1.0)*(double)rand()/((double)RAND_MAX + 1.0));
  sprintf(ObjectBuffer,"Write:group%ld/object%ld:TIMESTAMP:DOUBLE:%ld:",randgrp,randobj,randnb);
  for(i=0;i<(int)randnb;i++){
    val=(double)(((double)rand())/(double)RAND_MAX);
    sprintf(temp,"%lf:",val);
    strcat(ObjectBuffer,temp);
  }
  len=strlen(ObjectBuffer);
  ObjectBuffer[len-1]=';';
  //Write:group1/object1:TIMESTAMP:DOUBLE:10:1:2:3:4:5:6:7:8:9:10;
  //double val = (double)(((double)rand())/RAND_MAX); //random number from 0 to 1
  //n=6; j = (int)(n*rand()/(RAND_MAX + 1));//gives number 0-7
  log_chat (CHAT_LOG_FORMAT("%s\n**********************************\n"), LOG_HEADER,ObjectBuffer) ;
  log_chat (CHAT_LOG_FORMAT("nb_requested=%d,nb_of_vals_generated=%d\n"),
                  LOG_HEADER,(int)(len+1),(int)randnb) ;
  return len;
}

int output_to_file (OPTIONS_struct * op, char *ObjectBuffer, int len){
  len=len;
  fprintf(op->OutFile,"%s\n",ObjectBuffer);
  return OK;
}
int output_to_files (OPTIONS_struct * op, char *ObjectBuffer, int len){
  len=len;
  op=op;
  ObjectBuffer=ObjectBuffer;
  return OK;
}

int output_to_socket (OPTIONS_struct * op, char *ObjectBuffer, int len){
  //TODO: needs to be modified
  //TODO:Send ObjectBuffer to clients
  //op : structure with execution parameters (carries all the options for sockets)
  //ObjectBuffer: string of length len (with null character at end)

  int sent_length;
  //len+1 because we send also end character '\0'
  sent_length = send(op->OutSocket,(char *)ObjectBuffer, len+1,0);
  log_chat (CHAT_LOG_FORMAT("nb_sent=%d,nb_requested=%d\n"),LOG_HEADER,(int)sent_length,(int)(len+1)) ;
  if((int)sent_length == (int)(len+1)) return OK;
  else return NOT_OK;
  return OK;
}

int output_data (OPTIONS_struct * op, char *ObjectBuffer, int len  ){
  int ret;
  switch (op->option_output){
    case aSOCKET:
      ret = output_to_socket (op, ObjectBuffer, len);
      break ;
    case aFILE:
      ret=output_to_file (op, ObjectBuffer, len);
      break ;
    case aFILES:
      ret=output_to_files (op, ObjectBuffer, len);
      break ;
    default:
      return NOT_OK;
  }
  if(ret != OK){
      log_fatal (FATAL_LOG_FORMAT("Problem outputing data\n"), LOG_HEADER) ;
      return NOT_OK;
  }
  op->NbSetsSent ++;NbSetsSent ++;
  return OK;
}
/*
TMTCPacket structure:
  int full_pkt_size; // actual size of the packet, not PUS standard size
  unsigned char full_pkt [FULL_PKT_SIZE] ;
//Packet Header (48 bits)
  unsigned char header [TMTC_PKT_HEADER_SIZE];
//Packet ID
  int version_number;//version;
  int is_a_TM; // TM or TC flag
  int has_data; // data field header flag, packet contains data or not flag
  int apid;
//Packet Sequence Control
  int sequence_flags;
  int sequence_count;
//Packet Length
  int packet_length;
  int data_length;
//Packet Data Field
  int header_length;
  unsigned char data_header[TMTC_DATA_HEADER_SIZE] ;
  unsigned char data [TMTC_DATA_SIZE] ;
//Data Field Header
  int type;
  int sub_type;
//For TMs (telemetry) only
  int seconds;
  int micros;
//Application (TCs) or Source Data (TMs)
  int sid;
//For TCs (telecommands) only
  int p01;
  int fid_aid;
//Info from MIBs
  char uniq_name [MEDIUM_STR_LEN];
  char description [MEDIUM_STR_LEN];
//For TMs (telemetry) only
  int fid;
//For TCs (telecommands) only
  char short_desc [MEDIUM_STR_LEN];
//All effective parameters for the current packet 
  int nb_params; // number of parameters 
  ParameterValues parameter [NB_MAX_PARAMS_IN_PUS_PKT];
*/
/*
DATA_struct:
  int action; //type of action to perform
  char grp_obj_name[MAX_SIZE_FULLNAME];
  char grp_name[MAX_SIZE_FULLNAME];
  char obj_name[MAX_SIZE_FULLNAME];
  char full_name[MAX_SIZE_FULLNAME];
  int structure_type;
  int data_type;
  int num_vals;
//change to union????????????????????????????????????
  char     byte_val [MAX_NUM_VALS];
  unsigned char     flag_val [MAX_NUM_VALS];
  short   short_val [MAX_NUM_VALS];
  int       int_val [MAX_NUM_VALS];
  long     long_val [MAX_NUM_VALS];
  float   float_val [MAX_NUM_VALS];
  double double_val [MAX_NUM_VALS];
  //PIOSTRING string_val;
*/
int treat_PUS_packet (TMTCPacket* pkt, DATA_struct idata, OPTIONS_struct * op ){
  int ret;
  //unsigned int usecs=2;
  //usleep(usecs);

  //Verify how packets are differentiated
  if(!pkt->is_a_TM){
    //Case TC (Telecommad packets)
    //Produce listing!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    return OK;
  }else{
    if(pkt->type==21){
      //Case Sci (Science packets)
      //Use parameters: data:   pointer to start of Packet Data Field = (start of packet + 6 octets)
      //                length: length of packet in octets - Packet Header length (6 octets) + 10 octets:
      //ret = DecompressHandler ((unsigned short *)((pkt->full_pkt)+6), pkt->full_pkt_size-6, op->DPU_version, pkt, idata);
      ret = DecompressHandler ((unsigned short *)((pkt->full_pkt)+16), pkt->full_pkt_size-16, op->DPU_version, pkt, 
                               idata, op );
    } else {
      //Case HK (Houskeeping packets)
      ret = HskHandler (pkt, idata, op );
    }
  }

 return OK;
}

int loop_simulated (DATA_struct idata, OPTIONS_struct * op  ){
  int ret;
  char ObjectBuffer[20000];
  int len;
  idata=idata;

  //main loop with code to write on socket or file
  //data is real data or simulated data
  //socket code needs to be changed !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  while(1) {
    if(op->option_output==aSOCKET){
      // open socket
      //ret = connect_to_output_socket (op);
    }
    ret = OK;
    while (ret==OK) {
      //send data here!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      len = GenerateSimulatedData (ObjectBuffer);
      ret = output_data (op, ObjectBuffer, len);
      if(ret == OK){ 
          op->NbSetsSent ++;NbSetsSent ++;
      }else{
          log_fatal (FATAL_LOG_FORMAT("Problem outputing data\n"), LOG_HEADER) ;
          continue;
      }
    }
    if(op->option_output==aSOCKET){
        log_chat (CHAT_LOG_FORMAT("Sending to socket %d ok\n"), LOG_HEADER,
                                   op->OutSocket) ;
        if(op->OutSocket>-1)close(op->OutSocket);
        op->OutSocket=-1;
    }
  }
  return OK;
}
//#define READ_BUFF_SIZE 1100 already defined in MIB_structs.h
int loop_from_file (DATA_struct idata, OPTIONS_struct * op, hsearch_data_struct htab ){
  int ret;
  FILE * inputFile = NULL;
  inputFile = openFile ( op->InFileName, "r" ) ;
  if ( inputFile == NULL ) exit ( 1 ) ;

  //Loop to read packets (similar to ~/planck/HL1/Lib_pkg/HL1_PUSlib2/src/test.c)
  //Needs to be re-written when buffering library available !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  //  TMTCPacket* pkt = new_TMTCPacket();
  TMTCPacket* pkt = new_TMTCPacket();
  char to_read [READ_BUFF_SIZE] ;
  char *pkt_begin ;
  int state = 0 ;
  int size_to_read = READ_BUFF_SIZE ;
  int size_to_move = 0 ;
  int offset = 0 ;
  int go_on = TRUE;
  //main loop with code to write on socket or file
  //data is real data or simulated data
  //socket code needs to be changed !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  while(go_on==TRUE) {
    if(op->option_output==aSOCKET){
      //TODO: needs to be modified
      //Here we open the socket/socketss where data is to be sent
      //ret = connect_to_output_socket (op);
    }
    while (size_to_read != 0 && (read(fileno(inputFile), (void*) (to_read + offset), size_to_read) == size_to_read)) {
      state = readPUS_get_one_pkt(pkt, to_read, READ_BUFF_SIZE, &pkt_begin, &htab) ;
      switch (state) {
      case FULL_PKT:
        // print_TMTCPacket (pkt, stdout) ;
        // treat packet here 
        ret = treat_PUS_packet ( pkt, idata, op );
        if(ret==NOT_OK){
          log_fatal( FATAL_LOG_FORMAT("Aborting\n"),LOG_HEADER) ;
          exit(1) ;
        }
        size_to_move = to_read + READ_BUFF_SIZE -	(pkt_begin + pkt->full_pkt_size) ;
        size_to_read = READ_BUFF_SIZE - size_to_move ;
        memmove (to_read, pkt_begin + pkt->full_pkt_size, size_to_move) ;
        offset = size_to_move ;
        break;
      case PKT_BEGIN:
        /* pkt->full_pkt_size is not yet known here */
        size_to_move = to_read + READ_BUFF_SIZE - pkt_begin ;
        size_to_read = READ_BUFF_SIZE - size_to_move ;
        memmove (to_read, pkt_begin, size_to_move) ;
        offset = size_to_move ;
        break;
      case GARBAGE:
        size_to_read = READ_BUFF_SIZE ;
        offset = 0;
        break;
      default :
        log_fatal( FATAL_LOG_FORMAT("unexpected return state:%d\n"),LOG_HEADER, state) ;
        exit(1) ;
      }
    }
    if(op->option_input  == aFILE)go_on=FALSE;
    state = readPUS_get_one_pkt(pkt, to_read, READ_BUFF_SIZE, &pkt_begin, &htab) ;
    if (state == FULL_PKT) {
      // print_TMTCPacket (pkt, stdout) ;
      // treat last packet here (if it is complete)
      ret = treat_PUS_packet ( pkt, idata, op );
      if(ret==NOT_OK){
        log_fatal( FATAL_LOG_FORMAT("Aborting\n"),LOG_HEADER) ;
        exit(1) ;
      }
    }
  } 
  /*End Read PUS packets*/
  if(op->option_output==aSOCKET){
      //TODO: needs to be changed
      //Close output socket
      log_chat (CHAT_LOG_FORMAT("Sending to socket %d ok\n"), LOG_HEADER,
                                 op->OutSocket) ;
      if(op->OutSocket>-1)close(op->OutSocket);
      op->OutSocket=-1;
  }
  fclose(inputFile);
  log_close() ;
  free(pkt);

  return OK;
}
int loop_from_files (DATA_struct idata, OPTIONS_struct * op, hsearch_data_struct htab ){
  //Get file names from file InFileNameList. For each, set op->InFileName  and call loop_from_file
  idata=idata;
  op=op;
  htab=htab;
  return OK;
}
int loop_from_socket (DATA_struct idata, OPTIONS_struct * op, hsearch_data_struct htab ){
  idata=idata;
  op=op;
  htab=htab;
  //TODO: needs to be changed
  //Same as loop_from_file but instead of opening a file and reading from it we need to open a socket and get data
  //that is being sent
  return OK;
}

int main( int argc, char **argv ){
  int ret;
  //Initialisations
  OPTIONS_struct *op;
  op=(OPTIONS_struct *)malloc(sizeof(OPTIONS_struct));
  int saveerrno = errno;
  if(op==NULL){
          log_fatal (FATAL_LOG_FORMAT("Can't malloc op: %s\n"), LOG_HEADER, strerror( saveerrno )) ;
          exit (0);
  }
  HandleOptions( argc, argv, op ) ;

  set_progname(argv[0], __FILE__); /* method from man:errormsg */
  if ( strcmp (op->LogName, "stdout" ) != 0 )log_set_localtime( log_open ( op->LogName, log_level ), 1) ;
  else                                      log_set_localtime( log_add  ( stdout,     log_level ), 1) ;

  if ( op->option_send_elog == aYES ) elogStart() ;

  op->OutFile = openFile ( op->OutFileName, "w" ) ;
  if ( op->OutFile == NULL ) exit ( 1 ) ;

  //signal management that needs to be changed !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  signal( SIGPIPE, sig_broken_pipe ) ; // catch broken pipe
  signal( SIGALRM, sig_reveil ) ;
  signal( SIGQUIT, sig_kill ) ;
  signal( SIGINT, sig_kill ) ;
  signal( SIGUSR1, sig_usr ) ;
  signal( SIGUSR2, sig_usr2 ) ;

/*Read MIBs*/
  //Need to create packet and parameter tables (so that they are controled by the user)
  known_packet packet_desc_table [NB_MAX_PKT_IN_MIBS] ;
  parameter_desc param_desc_table [NB_MAX_PARAMS_IN_MIBS] ;

  //Need to create hash table (so that it is controled by the user).
  hsearch_data_struct htab;
  htab.table=NULL;
  htab.size=0;
  htab.filled=0;

  //Read ccf.dat, cdf.dat, cpc.dat, pid.dat, plf.dat, pcf.dat
  //   and fill packet and parameter tables
  //   and creat hash table
  ret = get_MIB_info(op->MIBPath, packet_desc_table, param_desc_table, &htab);
  if(ret == NOT_OK){
    log_error (ERROR_LOG_FORMAT("MIB info initialisation failed\n"),
               LOG_HEADER) ;
    exit(1);
  }
/*End Read MIBs*/


  //Initialise output EXCHANGE structure
  DATA_struct idata;
  ret = init_structure ( idata );

  //Input data is real data or simulated data
  //If real data the from file, from list of files or from a socket
  signal( SIGQUIT, sig_tst_hand ) ;
  signal( SIGINT, sig_tst_hand ) ;
  if(op->option_input  == aSIMUL){
    ret=loop_simulated (idata, op);
  }else{
    switch( op->option_input ) {
      case aFILE: ret=loop_from_file (idata, op, htab);
        break ;
      case aFILES: ret=loop_from_files (idata, op, htab);
        break ;
      case aSOCKET: ret=loop_from_socket (idata, op, htab);
        break ;
      default: Help(op) ;
    }
  }
  exit( 0 ) ;
}
