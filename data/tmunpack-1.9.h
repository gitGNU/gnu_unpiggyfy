// written to allow gcc -fsyntax-only tmunpack-1.9.c to work fine

typedef struct {
  char Options [1024] ;
  time_t StartTime ;
  FILE* LogFile ;
  char* HostName ;
  char* UserName ;
  int DPU_version ;
  char ElogCmd [128] ;
  char elogFormat [128] ;
  char* ElogMsg ;
  char ElogMsgFile [128] ;
  char* ElogServer ;
  int EndTime ;
  char* InFileName ;
  char* InFileNameList ;
  char* InHost ;
  int InPort ;
  int input_client_buff ;
  int input_read_timeout_ms ;
  int InSocket ;
  int log_level ;
  char* LogName ;
  int MaxDate ;
  char* MIBPath ;
  int MinDate ;
  int NbItemsFound ;
  int NbSetsSent ;
  int option_input ;
  int option_output ;
  int option_send_elog ;
  FILE* OutFile ;
  char* OutFileName ;
  char* OutHost ;
  int OutPort ;
  char* OutputDir ;
  int output_do_not_lose_data ;
  int output_each_client_buffer_size ;
  int output_maximum_clients_number ;
  int output_send_timeout_ms ;
  int output_server_buffer_size ;
  int OutSocket ;
  int RetryWait ;
  double SamplingDivisor ;
} OPTIONS_struct ;

typedef struct {
  int is_a_TM ;
  int type ;
  char* full_pkt ;
  int full_pkt_size ;
} TMTCPacket ;

typedef struct {
} DATA_struct ;

typedef struct {
  int size ;
  int filled ;
  void* table ;
} hsearch_data_struct ;

typedef struct {
} known_packet ;

typedef struct {
} parameter_desc ;

#define OK 1
#define NOT_OK 0
#define READ_BUFF_SIZE 2048
#define TRUE 1
#define FALSE 0
#define FULL_PKT 1024
#define PKT_BEGIN 6
#define GARBAGE 3
#define NB_MAX_PKT_IN_MIBS 3000
#define NB_MAX_PARAMS_IN_MIBS 10000

FILE* openFile (char* name, char* mode) ;
TMTCPacket* new_TMTCPacket() ;

#define LOG_HEADER __FILE__,__PRETTY_FUNCTION__,__LINE__

#define CHAT_PREFIX "Chat:%s:%s:line %d:"
#define FATAL_PREFIX "Fatal:%s:%s:line %d:"

#define CHAT_LOG_FORMAT(USER_FORMAT) CHAT_PREFIX USER_FORMAT
#define FATAL_LOG_FORMAT(USER_FORMAT) FATAL_PREFIX USER_FORMAT

void log_chat (const char *, ...);
void log_fatal (const char *, ...);
