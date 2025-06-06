#include "../../util/fd_util.h"
#include "../../ballet/base64/fd_base64.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>
#include <time.h>
#include <signal.h>
#include <unistd.h>
#include <stdarg.h>
#include <strings.h>
#include <sys/types.h>
#include <sys/socket.h>
#include "fd_methods.h"
#include "fd_webserver.h"
#include "../../ballet/http/fd_http_server_private.h"

struct fd_websocket_ctx {
  fd_webserver_t * ws;
  ulong connection_id;
};

static void
fd_web_reply_flush( fd_webserver_t * ws ) {
  if( ws->quick_size ) {
    fd_http_server_memcpy(ws->server, (const uchar*)ws->quick_buf, ws->quick_size);
    ws->quick_size = 0;
  }
}

void
fd_web_reply_new( fd_webserver_t * ws ) {
  ws->quick_size = 0;
  fd_http_server_stage_trunc( ws->server, 0 );
  ws->prev_reply_len = 0;
  ws->status_code = 200; // OK
}

// Parse the top level json request object
static void
json_parse_root(fd_webserver_t * ws, json_lex_state_t* lex) {
  struct json_path path;
  if (json_lex_next_token(lex) == JSON_TOKEN_LBRACKET) {
    /* We have an array of requests */
    fd_web_reply_append(ws, "[", 1);
    while(1) {
      fd_web_reply_flush( ws );
      ws->prev_reply_len = fd_http_server_stage_len( ws->server );

      struct json_values values;
      json_values_new(&values);
      path.len = 0;
      if (json_values_parse(lex, &values, &path)) {
        fd_webserver_method_generic(&values, ws->cb_arg);
      } else {
        ulong sz;
        const char* text = json_lex_get_text(lex, &sz);
        char text2[4096];
        snprintf( text2, sizeof(text2), "Parse error: %s", text );
        fd_web_reply_error( ws, -1, text2, "null" );
        json_values_delete(&values);
        break;
      }
      json_values_delete(&values);

      long tok = json_lex_next_token(lex);
      if( tok == JSON_TOKEN_COMMA ) {
        fd_web_reply_append(ws, ",", 1);
      } else if( tok == JSON_TOKEN_RBRACKET ) {
        break;
      } else {
        fd_web_reply_append(ws, ",", 1);
        fd_web_reply_flush( ws );
        ws->prev_reply_len = fd_http_server_stage_len( ws->server );
        fd_web_reply_error( ws, -1, "Parse error: missing , or ]", "null" );
        break;
      }
    }
    fd_web_reply_append(ws, "]", 1);

  } else {
    /* Go back to the first token */
    lex->pos = 0;
    lex->last_tok = JSON_TOKEN_ERROR;

    struct json_values values;
    json_values_new(&values);
    path.len = 0;
    if (json_values_parse(lex, &values, &path)) {
      fd_webserver_method_generic(&values, ws->cb_arg);
    } else {
      ulong sz;
      const char* text = json_lex_get_text(lex, &sz);
      char text2[4096];
      snprintf( text2, sizeof(text2), "Parse error: %s", text );
      fd_web_reply_error( ws, -1, text2, "null" );
    }
    json_values_delete(&values);
  }
}

void
fd_web_reply_error( fd_webserver_t * ws, int errcode, const char * text, const char * call_id ) {
  ws->quick_size = 0;
  fd_http_server_stage_trunc(ws->server, ws->prev_reply_len);
  fd_web_reply_sprintf(ws, "{\"jsonrpc\":\"2.0\",\"error\":{\"code\":%d,\"message\":", errcode);
  fd_web_reply_encode_json_string(ws, text);
  fd_web_reply_sprintf(ws, "},\"id\":%s}", call_id );
}

static void
fd_web_protocol_error( fd_webserver_t * ws, const char* text ) {
#define CRLF "\r\n"
  static const char* DOC1 =
"<html>" CRLF
"<head>" CRLF
"<title>ERROR</title>" CRLF
"</head>" CRLF
"<body>" CRLF
"<p><em>";
  static const char* DOC2 =
"</em></p>" CRLF
"</body>" CRLF
"</html>" CRLF;

  fd_web_reply_new(ws);
  fd_http_server_memcpy(ws->server, (const uchar*)DOC1, strlen(DOC1));
  fd_http_server_memcpy(ws->server, (const uchar*)text, strlen(text));
  fd_http_server_memcpy(ws->server, (const uchar*)DOC2, strlen(DOC2));

  ws->status_code = 400; // BAD_REQUEST
}

static fd_http_server_response_t
request( fd_http_server_request_t const * request ) {
  fd_webserver_t * ws = (fd_webserver_t *)request->ctx;
  fd_web_reply_new( ws );

  if( FD_LIKELY( request->method==FD_HTTP_SERVER_METHOD_GET ) ) {
    if( FD_LIKELY( request->headers.upgrade_websocket ) ) {
      fd_http_server_response_t response = {
        .status            = 200,
        .upgrade_websocket = 1,
        .content_type      = "application/json",
      };
      return response;
    }

    fd_web_protocol_error( ws, "GET method not supported!" );

    fd_http_server_response_t response = {
      .status            = 400,
      .upgrade_websocket = 0,
      .content_type      = "text/html",
    };
    FD_TEST( !fd_http_server_stage_body( ws->server, &response ) );
    return response;

  } else if( request->method==FD_HTTP_SERVER_METHOD_OPTIONS ) {
    fd_http_server_response_t response = {
      .status                       = 204UL,
      .upgrade_websocket            = 0,
      .content_type                 = NULL,
      .access_control_allow_origin  = "*",
      .access_control_allow_methods = "POST, GET, OPTIONS",
      .access_control_allow_headers = "*",
      .access_control_max_age       = 86400,
    };
    return response;

  } else {
    if( strcmp(request->path, "/") != 0 ) {
      fd_web_protocol_error( ws, "POST path must be \"/\"" );

    } else if( strncasecmp(request->headers.content_type, "application/json", 16) != 0 ) {
      fd_web_protocol_error( ws, "content type must be \"application/json\"" );

    } else {
#ifdef FD_RPC_DEBUG
      fwrite((const char*)request->post.body, 1, request->post.body_len, stdout);
      fwrite("\n", 1, 1, stdout);
      fflush(stdout);
#endif
      FD_SPAD_FRAME_BEGIN( ws->spad ) {
        json_lex_state_t lex;
        json_lex_state_new(&lex, (const char*)request->post.body, request->post.body_len, ws->spad);
        json_parse_root(ws, &lex);
        json_lex_state_delete(&lex);
        fd_web_reply_flush( ws );
      } FD_SPAD_FRAME_END;
    }

    fd_http_server_response_t response = {
      .status            = ws->status_code,
      .upgrade_websocket = 0,
      .content_type      = ( ws->status_code == 200 ? "application/json" : "text/html" ),
      .access_control_allow_origin = "*",
    };
    if( FD_UNLIKELY( fd_http_server_stage_body( ws->server, &response ) ) ) {
      FD_LOG_WARNING(( "fd_http_server_stage_body failed" ));
      fd_http_server_response_t response = {
        .status                      = 500,
        .upgrade_websocket           = 0,
        .content_type                = "text/html",
        .access_control_allow_origin = "*",
      };
      return response;
    }
#ifdef FD_RPC_DEBUG
    fwrite(ws->server->oring + response._body_off, 1, response._body_len, stdout);
    fwrite("\n", 1, 1, stdout);
    fflush(stdout);
#endif
    return response;
  }
}

static void
http_open( ulong connection_id, int sockfd, void * ctx ) {
  (void)connection_id;
  (void)ctx;

  int newsize = 1<<20;
  int rc = setsockopt(sockfd, SOL_SOCKET, SO_SNDBUF, &newsize, sizeof(newsize));
  if( FD_UNLIKELY( -1==rc ) ) FD_LOG_ERR(( "setsockopt failed (%i-%s)", errno, strerror( errno ) )); /* Unexpected programmer error, abort */
}

static void
http_close( ulong connection_id, int reason, void * ctx ) {
  (void)connection_id;
  (void)reason;
  (void)ctx;
}

static void
ws_open( ulong connection_id, void * ctx ) {
  (void)connection_id;
  (void)ctx;
}

static void
ws_close( ulong connection_id, int reason, void * ctx ) {
  (void)reason;
  fd_webserver_t * ws = (fd_webserver_t *)ctx;
  fd_webserver_ws_closed( connection_id, ws->cb_arg );
}

static void
ws_message( ulong conn_id, uchar const * data, ulong data_len, void * ctx ) {
#ifdef FD_RPC_VERBOSE
  fwrite("message:\n\n", 1, 9, stdout);
  fwrite(data, 1, data_len, stdout);
  fwrite("\n\n", 1, 2, stdout);
  fflush(stdout);
#endif

  fd_webserver_t * ws = (fd_webserver_t *)ctx;
  fd_web_reply_new( ws );

  json_lex_state_t lex;
  json_lex_state_new(&lex, (const char*)data, data_len, ws->spad);
  struct json_values values;
  json_values_new(&values);
  struct json_path path;
  path.len = 0;
  int ret = json_values_parse(&lex, &values, &path);
  if (ret) {
    // json_values_printout(&values);
    ret = fd_webserver_ws_subscribe(&values, conn_id, ws->cb_arg);
  } else {
    ulong sz;
    const char* text = json_lex_get_text(&lex, &sz);
    char text2[4096];
    snprintf( text2, sizeof(text2), "Parse error: %s", text );
    fd_web_reply_error( ws, -1, text2, "null" );
  }
  json_values_delete(&values);
  json_lex_state_delete(&lex);
  fd_web_ws_send( ws, conn_id );
}

void fd_web_ws_send( fd_webserver_t * ws, ulong conn_id ) {
  fd_web_reply_flush( ws );
  fd_http_server_ws_send( ws->server, conn_id );
}

int fd_webserver_start( ushort portno, fd_http_server_params_t params, fd_spad_t * spad, fd_webserver_t * ws, void * cb_arg ) {
  memset(ws, 0, sizeof(fd_webserver_t));

  ws->cb_arg = cb_arg;
  ws->spad = spad;

  fd_http_server_callbacks_t callbacks = {
    .request    = request,
    .open       = http_open,
    .close      = http_close,
    .ws_open    = ws_open,
    .ws_close   = ws_close,
    .ws_message = ws_message,
  };

  void* server_mem = fd_spad_alloc( spad, fd_http_server_align(), fd_http_server_footprint( params ) );
  ws->server = fd_http_server_join( fd_http_server_new( server_mem, params, callbacks, ws ) );

  FD_TEST( fd_http_server_listen( ws->server, 0, portno ) != NULL );

  return 0;
}

int
fd_webserver_poll(fd_webserver_t * ws) {
  return fd_http_server_poll( ws->server, 0 );
}

int
fd_webserver_fd(fd_webserver_t * ws) {
  return fd_http_server_fd( ws->server );
}

int
fd_web_reply_append( fd_webserver_t * ws,
                     const char *     text,
                     ulong            text_sz ) {
  if( FD_LIKELY( ws->quick_size + text_sz <= FD_WEBSERVER_QUICK_MAX ) ) {
    memcpy( ws->quick_buf + ws->quick_size, text, text_sz );
    ws->quick_size += text_sz;
  } else {
    fd_web_reply_flush( ws );
    if( FD_LIKELY( text_sz <= FD_WEBSERVER_QUICK_MAX ) ) {
      memcpy( ws->quick_buf, text, text_sz );
      ws->quick_size = text_sz;
    } else {
      fd_http_server_memcpy( ws->server, (const uchar*)text, text_sz );
    }
  }
  return 0;
}

static const char b58digits_ordered[] = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz";

int
fd_web_reply_encode_base58( fd_webserver_t * ws,
                            const void *     data,
                            ulong            data_sz ) {
  /* Prevent explosive growth in computation */
  if (data_sz > 400U)
    return -1;

  const uchar* bin = (const uchar*)data;
  ulong carry;
  ulong i, j, high, zcount = 0;
  ulong size;

  while (zcount < data_sz && !bin[zcount])
    ++zcount;

  /* Temporary buffer size */
  size = (data_sz - zcount) * 138 / 100 + 1;
  uchar buf[size];
  memset(buf, 0, size);

  for (i = zcount, high = size - 1; i < data_sz; ++i, high = j) {
    for (carry = bin[i], j = size - 1; (j > high) || carry; --j) {
      carry += 256UL * (ulong)buf[j];
      buf[j] = (uchar)(carry % 58);
      carry /= 58UL;
      if (!j) {
        // Otherwise j wraps to maxint which is > high
        break;
      }
    }
  }

  for (j = 0; j < size && !buf[j]; ++j) ;

  ulong out_sz = zcount + size - j;
  char b58[out_sz];
  if (zcount)
    fd_memset(b58, '1', zcount);
  for (i = zcount; j < size; ++i, ++j)
    b58[i] = b58digits_ordered[buf[j]];

  return fd_web_reply_append( ws, b58, out_sz );
}

static char base64_encoding_table[] = {
  'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H',
  'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P',
  'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X',
  'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f',
  'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n',
  'o', 'p', 'q', 'r', 's', 't', 'u', 'v',
  'w', 'x', 'y', 'z', '0', '1', '2', '3',
  '4', '5', '6', '7', '8', '9', '+', '/'
};

int
fd_web_reply_encode_base64( fd_webserver_t * ws,
                            const void *     data,
                            ulong            data_sz ) {
  for (ulong i = 0; i < data_sz; ) {
    if( FD_UNLIKELY( ws->quick_size + 4U > FD_WEBSERVER_QUICK_MAX ) ) {
      fd_web_reply_flush( ws );
    }
    char * out_data = ws->quick_buf + ws->quick_size;
    switch (data_sz - i) {
    default: { /* 3 and above */
      uint octet_a = ((uchar*)data)[i++];
      uint octet_b = ((uchar*)data)[i++];
      uint octet_c = ((uchar*)data)[i++];
      uint triple = (octet_a << 0x10) + (octet_b << 0x08) + octet_c;
      out_data[0] = base64_encoding_table[(triple >> 3 * 6) & 0x3F];
      out_data[1] = base64_encoding_table[(triple >> 2 * 6) & 0x3F];
      out_data[2] = base64_encoding_table[(triple >> 1 * 6) & 0x3F];
      out_data[3] = base64_encoding_table[(triple >> 0 * 6) & 0x3F];
      break;
    }
    case 2: {
      uint octet_a = ((uchar*)data)[i++];
      uint octet_b = ((uchar*)data)[i++];
      uint triple = (octet_a << 0x10) + (octet_b << 0x08);
      out_data[0] = base64_encoding_table[(triple >> 3 * 6) & 0x3F];
      out_data[1] = base64_encoding_table[(triple >> 2 * 6) & 0x3F];
      out_data[2] = base64_encoding_table[(triple >> 1 * 6) & 0x3F];
      out_data[3] = '=';
      break;
    }
    case 1: {
      uint octet_a = ((uchar*)data)[i++];
      uint triple = (octet_a << 0x10);
      out_data[0] = base64_encoding_table[(triple >> 3 * 6) & 0x3F];
      out_data[1] = base64_encoding_table[(triple >> 2 * 6) & 0x3F];
      out_data[2] = '=';
      out_data[3] = '=';
      break;
    }
    }
    ws->quick_size += 4U;
  }
  return 0;
}

static const char hex_encoding_table[] = "0123456789ABCDEF";

int
fd_web_reply_encode_hex( fd_webserver_t * ws,
                         const void *     data,
                         ulong            data_sz ) {
  for (ulong i = 0; i < data_sz; ) {
    if( FD_UNLIKELY( ws->quick_size + 2U > FD_WEBSERVER_QUICK_MAX ) ) {
      fd_web_reply_flush( ws );
    }
    char * out_data = ws->quick_buf + ws->quick_size;
    uint octet = ((uchar*)data)[i++];
    out_data[0] = hex_encoding_table[(octet >> 4) & 0xF];
    out_data[1] = hex_encoding_table[octet & 0xF];
    ws->quick_size += 2U;
  }
  return 0;
}

int
fd_web_reply_sprintf( fd_webserver_t * ws, const char* format, ... ) {
  ulong remain = FD_WEBSERVER_QUICK_MAX - ws->quick_size;
  char * buf = ws->quick_buf + ws->quick_size;
  va_list ap;
  va_start(ap, format);
  int r = vsnprintf(buf, remain, format, ap);
  va_end(ap);
  if( FD_UNLIKELY( r < 0 ) ) return -1;
  if( FD_LIKELY( (uint)r < remain ) ) {
    ws->quick_size += (uint)r;
    return 0;
  }

  fd_web_reply_flush( ws );
  buf = ws->quick_buf;
  va_start(ap, format);
  r = vsnprintf(buf, FD_WEBSERVER_QUICK_MAX, format, ap);
  va_end(ap);
  if( r < 0 || (uint)r >= FD_WEBSERVER_QUICK_MAX ) return -1;
  ws->quick_size = (uint)r;
  return 0;
}

int
fd_web_reply_encode_json_string( fd_webserver_t * ws, const char * str ) {
  char buf[512];
  buf[0] = '"';
  ulong buflen = 1;
  while( *str ) {

    /* UTF-8 decode */
    uint c = (uchar)(*str);
    uint k = (uint)__builtin_clz(~(c << 24U)); // Count # of leading 1 bits.
    /* k = 0 for one-byte code points; otherwise, k = #total bytes. */
    uint value = c;
    if( k ) {
      value &= (1U << (8U - k)) - 1U;          // All 1s with k+1 leading 0s.
      for ((c = (uchar)(*(++str))), --k; k > 0; --k, (c = (uchar)(*(++str)))) {
        /* tests if a char is a continuation byte in utf8. */
        if( (c & 0xc0U) != 0x80U ) return -1;
        value <<= 6;
        value += (c & 0x3FU);
      }
    }

    switch( value ) {
    case (uchar)'\\': buf[buflen++] = '\\'; buf[buflen++] = '\\'; break;
    case (uchar)'\"': buf[buflen++] = '\\'; buf[buflen++] = '\"'; break;
    case (uchar)'\n': buf[buflen++] = '\\'; buf[buflen++] = 'n';  break;
    case (uchar)'\t': buf[buflen++] = '\\'; buf[buflen++] = 't';  break;
    case (uchar)'\r': buf[buflen++] = '\\'; buf[buflen++] = 'r';  break;
    default:
      if( value >= 0x20 && value <= 0x7F ) {
        buf[buflen++] = (char)value;
      } else {
        buflen += (uint)snprintf(buf + buflen, sizeof(buf) - buflen - 1U, "\\u%04x", value);
      }
    }

    if( buflen >= sizeof(buf)-10U ) {
      int err = fd_web_reply_append( ws, buf, buflen );
      if( err ) return err;
      buflen = 0;
    }

    ++str;
  }
  buf[buflen++] = '"';
  return fd_web_reply_append( ws, buf, buflen );
}
