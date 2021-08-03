/* Scanner
 * @copyright (c) 2008, Hedspi, Hanoi University of Technology
 * @author Huu-Duc Nguyen
 * @version 1.0
 */

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

#include "reader.h"
#include "charcode.h"
#include "token.h"
#include "error.h"
#include "scanner.h"

extern int lineNo;
extern int colNo;
extern int currentChar;

extern CharCode charCodes[];

/***************************************************************/

// void skipBlank() {
//   while ((currentChar != EOF) && (charCodes[currentChar] == CHAR_SPACE))
//     readChar();
// }

// void skipComment() {
//   int state = 0;
//   while ((currentChar != EOF) && (state < 2)) {
//     switch (charCodes[currentChar]) {
//     case CHAR_TIMES:
//       state = 1;
//       break;
//     case CHAR_RPAR:
//       if (state == 1) state = 2;
//       else state = 0;
//       break;
//     default:
//       state = 0;
//     }
//     readChar();
//   }
//   if (state != 2)
//     error(ERR_END_OF_COMMENT, lineNo, colNo);
// }

// Token* readIdentKeyword(void) {
//   Token *token = makeToken(TK_NONE, lineNo, colNo);
//   int count = 1;

//   token->string[0] = toupper((char)currentChar);
//   readChar();

//   while ((currentChar != EOF) && 
// 	 ((charCodes[currentChar] == CHAR_LETTER) || (charCodes[currentChar] == CHAR_DIGIT))) {
//     if (count <= MAX_IDENT_LEN) token->string[count++] = toupper((char)currentChar);
//     readChar();
//   }

//   if (count > MAX_IDENT_LEN) {
//     error(ERR_IDENT_TOO_LONG, token->lineNo, token->colNo);
//     return token;
//   }

//   token->string[count] = '\0';
//   token->tokenType = checkKeyword(token->string);

//   if (token->tokenType == TK_NONE)
//     token->tokenType = TK_IDENT;

//   return token;
// }

// Token* readNumber(void) { // Integer + Double (1)
//   int ln, cn;
//   Token *token = makeToken(TK_NUMBER, lineNo, colNo);
//   int count = 0;

//   while ((currentChar != EOF) && (charCodes[currentChar] == CHAR_DIGIT)) {
//     token->string[count++] = (char) currentChar;
//     readChar();
//   }

//   token->string[count] = '\0';
//   token->value = atoi(token->string);
//   if (charCodes[currentChar] != CHAR_PERIOD)
//     return token;

//   /* Check double */
//   ln = lineNo;
//   cn = colNo;
//   readChar();
//   if (charCodes[currentChar] != CHAR_DIGIT) {
//     currentChar = '.';
//     lineNo = ln;
//     colNo = cn;
//     return token;
//   }

//   // Get double
//   token->string[count++] = '.';
//   while ((currentChar != EOF) && (charCodes[currentChar] == CHAR_DIGIT)) {
//     token->string[count++] = (char) currentChar;
//     readChar();
//   }
//   token->string[count] = '\0';
//   token->tokenType = TK_DOUBLE;
//   token->d_value = atof(token->string);
//   return token;
// }

// Token* readConstChar(void) {
//   Token *token = makeToken(TK_CHAR, lineNo, colNo);

//   readChar();
//   if (currentChar == EOF) {
//     token->tokenType = TK_NONE;
//     error(ERR_INVALID_CONSTANT_CHAR, token->lineNo, token->colNo);
//     return token;
//   }
    
//   token->string[0] = currentChar;
//   token->string[1] = '\0';

//   readChar();
//   if (currentChar == EOF) {
//     token->tokenType = TK_NONE;
//     error(ERR_INVALID_CONSTANT_CHAR, token->lineNo, token->colNo);
//     return token;
//   }

//   if (charCodes[currentChar] == CHAR_SINGLEQUOTE) {
//     readChar();
//     return token;
//   } else {
//     token->tokenType = TK_NONE;
//     error(ERR_INVALID_CONSTANT_CHAR, token->lineNo, token->colNo);
//     return token;
//   }
// }

// Token* readConstChar2(void) { // String (1)
//   Token *token = makeToken(TK_STRING, lineNo, colNo);
//   readChar();
//   if (charCodes[currentChar] == CHAR_DOUBLEQUOTE) { // Null string
//     readChar();
//     token->string[0] = '\0';
//     return token;
//   }

//   int n = 0;
//   while(charCodes[currentChar] == CHAR_LETTER || charCodes[currentChar] == CHAR_DIGIT) {
//     token->string[n++] = currentChar;
//     readChar();
//   }
//   if (charCodes[currentChar] == CHAR_DOUBLEQUOTE) {
//     readChar();
//     token->string[n++] = '\0';
//     return token;
//   }
//   else {
//     error(ERR_INVALID_CONSTANT_CHAR, token->lineNo, token->colNo);
//     token->tokenType = TK_NONE;
//     return token;
//   }
// }

void skipBlank() {
  // TODO
    do { 
        readChar(); 
        if(currentChar == EOF) break;
    } while(charCodes[currentChar] == CHAR_SPACE);
}

void skipComment() {
  // TODO
    do {
      while(charCodes[currentChar] != CHAR_TIMES) {
        readChar();
        if(currentChar == EOF) 
          error(ERR_END_OF_COMMENT, lineNo, colNo);
      }
      readChar();
      if(currentChar == EOF) 
        error(ERR_END_OF_COMMENT, lineNo, colNo);
    }while(charCodes[currentChar] != CHAR_RPAR);
    readChar();
}

Token* readIdentKeyword() {
  // TODO
    Token *token = makeToken(TK_NONE, lineNo, colNo);
    int i = 0;
    do {
        token->string[i++] = currentChar;
        readChar();
        if(i == MAX_IDENT_LEN + 1) error(ERR_IDENT_TOO_LONG, lineNo, colNo - i);
    } while(charCodes[currentChar] == CHAR_LETTER || charCodes[currentChar] == CHAR_DIGIT);
    token->string[i] = '\0';
    printf("hi\n");
    printf("%s\n",token->string);
    token->tokenType = checkKeyword(token->string);
    if (token->tokenType == TK_NONE)
      token->tokenType = TK_IDENT;
    return token;  
}

Token* readNumber(){
  Token *token = makeToken(TK_NUMBER, lineNo, colNo);
  int i=0;
  int flag=0;
  do{
    if(charCodes[currentChar]== CHAR_PERIOD){
      //printf("hi\n");
      
      readChar();
      token->tokenType = TK_DOUBLE;
      flag++;
    }
    if(flag>1){
      error(ERR_INVALID_CONSTANT_CHAR,lineNo, colNo);
    }
    token->string[i++]=currentChar;
    readChar();
  }while( charCodes[currentChar] == CHAR_PERIOD  || charCodes[currentChar]== CHAR_DIGIT);
  token->string[i] = '\0';
  return token;
}

Token* readConstChar(void) {
  // TODO
    Token *token = makeToken(TK_CHAR, lineNo, colNo);
    int i = colNo;
    readChar();
    if(currentChar == EOF || currentChar == '\n')
      error(ERR_INVALID_CONSTANT_CHAR, lineNo, i);
    *token->string = currentChar;
    readChar();
    if(charCodes[currentChar] != CHAR_SINGLEQUOTE) 
      error(ERR_INVALID_CONSTANT_CHAR, lineNo, i);
    readChar();
    return token;

}

Token* readString(){
  Token *token = makeToken(TK_STRING, lineNo, colNo);
  int i=0;
  do{
    readChar();
    token-> string[i++] = currentChar;
    if(currentChar == '\n' || currentChar == EOF)
      error(ERR_INVALID_CONSTANT_CHAR, lineNo, colNo - i); 
  }while(charCodes[currentChar] != CHAR_DOUBLEQUOTE);
  token->string[i-1]= '\0';
  readChar();
  return token; 
}

// Token* getToken(void) {
//   Token *token;
//   int ln, cn;

//   if (currentChar == EOF) 
//     return makeToken(TK_EOF, lineNo, colNo);

//   switch (charCodes[currentChar]) {
//   case CHAR_SPACE: skipBlank(); return getToken();
//   case CHAR_LETTER: return readIdentKeyword();
//   case CHAR_DIGIT: return readNumber();
//   case CHAR_PLUS:
//     ln = lineNo;
//     cn = colNo;
//     readChar();
//     if ((currentChar != EOF) && (charCodes[currentChar] == CHAR_EQ)){
//       readChar();
//       return makeToken(SB_ASSIGN_PLUS, ln, cn);
//     } else return makeToken(SB_PLUS, ln, cn);
//   case CHAR_MINUS:
//     ln = lineNo;
//     cn = colNo;
//     readChar();
//     if ((currentChar != EOF) && (charCodes[currentChar] == CHAR_EQ)){
//       readChar();
//       return makeToken(SB_ASSIGN_SUBTRACT, ln, cn);
//     } else return makeToken(SB_MINUS, lineNo, colNo);
//   case CHAR_TIMES:
//     ln = lineNo;
//     cn = colNo;
//     readChar();
//     if ((currentChar != EOF) && (charCodes[currentChar] == CHAR_EQ)){
//       readChar();
//       return makeToken(SB_ASSIGN_TIMES, ln, cn);
//     } else return makeToken(SB_TIMES, lineNo, colNo);
//   case CHAR_SLASH:
//     ln = lineNo;
//     cn = colNo;
//     readChar();
//     if ((currentChar != EOF) && (charCodes[currentChar] == CHAR_EQ)){
//       readChar();
//       return makeToken(SB_ASSIGN_DIVIDE, ln, cn);
//     } else return makeToken(SB_SLASH, lineNo, colNo);
//   case CHAR_LT:
//     ln = lineNo;
//     cn = colNo;
//     readChar();
//     if ((currentChar != EOF) && (charCodes[currentChar] == CHAR_EQ)) {
//       readChar();
//       return makeToken(SB_LE, ln, cn);
//     } else return makeToken(SB_LT, ln, cn);
//   case CHAR_GT:
//     ln = lineNo;
//     cn = colNo;
//     readChar();
//     if ((currentChar != EOF) && (charCodes[currentChar] == CHAR_EQ)) {
//       readChar();
//       return makeToken(SB_GE, ln, cn);
//     } else return makeToken(SB_GT, ln, cn);
//   case CHAR_EQ: 
//     token = makeToken(SB_EQ, lineNo, colNo);
//     readChar(); 
//     return token;
//   case CHAR_EXCLAIMATION:
//     ln = lineNo;
//     cn = colNo;
//     readChar();
//     if ((currentChar != EOF) && (charCodes[currentChar] == CHAR_EQ)) {
//       readChar();
//       return makeToken(SB_NEQ, ln, cn);
//     } else {
//       token = makeToken(TK_NONE, ln, cn);
//       error(ERR_INVALID_SYMBOL, ln, cn);
//       return token;
//     }
//   case CHAR_COMMA:
//     token = makeToken(SB_COMMA, lineNo, colNo);
//     readChar(); 
//     return token;
//   case CHAR_PERIOD:
//     token = makeToken(SB_PERIOD, lineNo, colNo);
//     readChar();
//     return token;
//   case CHAR_SEMICOLON:
//     token = makeToken(SB_SEMICOLON, lineNo, colNo);
//     readChar(); 
//     return token;
//   case CHAR_COLON:
//     ln = lineNo;
//     cn = colNo;
//     readChar();
//     if ((currentChar != EOF) && (charCodes[currentChar] == CHAR_EQ)) {
//       readChar();
//       return makeToken(SB_ASSIGN, ln, cn);
//     } else return makeToken(SB_COLON, ln, cn);
//   case CHAR_SINGLEQUOTE: return readConstChar();
//   case CHAR_DOUBLEQUOTE: return readConstChar2(); // (1)
//   case CHAR_LPAR:
//     ln = lineNo;
//     cn = colNo;
//     readChar();

//     if (currentChar == EOF) 
//       return makeToken(SB_LPAR, ln, cn);

//     if (charCodes[currentChar] != CHAR_TIMES)
//       return makeToken(SB_LPAR, ln, cn);

//     readChar();
//     skipComment();
//     return getToken();
    
//   case CHAR_RPAR:
//     token = makeToken(SB_RPAR, lineNo, colNo);
//     readChar(); 
//     return token;
//   case CHAR_OBRT:
//     token = makeToken(SB_OPEN_BRACKET, lineNo, colNo);
//     readChar();
//     return token;
//   case CHAR_CBRT:
//     token = makeToken(SB_CLOSE_BRACKET, lineNo, colNo);
//     readChar();
//     return token;
//   case CHAR_MODULUS:
//     token = makeToken(SB_MODULUS, lineNo, colNo);
//     readChar();
//     return token;
  
//   default:
//     token = makeToken(TK_NONE, lineNo, colNo);
//     error(ERR_INVALID_SYMBOL, lineNo, colNo);
//     readChar(); 
//     return token;
//   }
// }

Token* getToken(void) {
  Token *token;
  //int ln, cn;

  if (currentChar == EOF) 
    return makeToken(TK_EOF, lineNo, colNo);

  switch (charCodes[currentChar]) {
  case CHAR_SPACE: skipBlank(); return getToken();
  case CHAR_LETTER: return readIdentKeyword();
  case CHAR_DIGIT: return readNumber();
  case CHAR_PLUS: 
    token = makeToken(SB_PLUS, lineNo, colNo);
    readChar(); 
    if(charCodes[currentChar]== CHAR_DIGIT){
      //readNumber1();
    }
    return token;
    // ....
    // TODO
  case CHAR_MINUS:
    token = makeToken(SB_MINUS, lineNo, colNo);
    readChar();
    return token;
  
  case CHAR_TIMES:
    token = makeToken(SB_TIMES, lineNo, colNo);
    readChar();
    return token;
  
  case CHAR_SLASH:
    token = makeToken(SB_SLASH, lineNo, colNo);
    readChar();
    return token;
  
  case CHAR_LT: // <
    token = makeToken(SB_LT, lineNo, colNo);
    readChar();
    if(charCodes[currentChar] == CHAR_EQ) {
      token->tokenType = SB_LE;
      readChar();
    } 
    else if(charCodes[currentChar] == CHAR_GT) {
      token->tokenType = SB_NEQ;
      readChar();
    }
    return token;
  
  case CHAR_GT: // >
    token = makeToken(SB_GT, lineNo, colNo);
    // check >=
    readChar();
    if(charCodes[currentChar] == CHAR_EQ) {
      token->tokenType = SB_GE;
      readChar();
    }
    return token;
  
  case CHAR_EXCLAIMATION: // !
    readChar();
    if(charCodes[currentChar] == CHAR_EQ) {
      readChar();
      return makeToken(SB_NEQ, lineNo, colNo);
    }
    else error(ERR_INVALID_SYMBOL, lineNo, colNo);
  
  case CHAR_EQ: // =
    token = makeToken(SB_EQ, lineNo, colNo);
    readChar();
    return token;
  
  case CHAR_COMMA: //,
    token = makeToken(SB_COMMA, lineNo, colNo);
    readChar();
    return token;
  
  case CHAR_PERIOD: //.
    token = makeToken(SB_PERIOD, lineNo, colNo);
    readChar();
    if(charCodes[currentChar] == CHAR_DIGIT){
      token -> tokenType = TK_DOUBLE;
      token-> string[0]= '0';
      token-> string[1]= '.';
      int i=2;
      do{
        token -> string[i++]=currentChar;
        readChar();
        if(i == MAX_IDENT_LEN + 1) error(ERR_IDENT_TOO_LONG, lineNo, colNo - i);
      }while (charCodes[currentChar]== CHAR_DIGIT);
      token-> string[i]='\0';
    }
    return token;
  
  case CHAR_COLON: // :
    token= makeToken(SB_COLON, lineNo, colNo);
    readChar();
    if(charCodes[currentChar] == CHAR_EQ) {
      readChar();
      return makeToken(SB_ASSIGN, lineNo, colNo);
    } 
    
    return token;
  
  case CHAR_SEMICOLON: // ;
    token = makeToken(SB_SEMICOLON, lineNo, colNo);
    readChar();
    return token;
  
  case CHAR_LPAR: //(
  token= makeToken(SB_LPAR, lineNo, colNo);
    readChar();
    if(charCodes[currentChar] == CHAR_TIMES) {
        skipComment();
        return getToken();
    } 
    if(charCodes[currentChar]== CHAR_PERIOD){
      readChar();
    }
    return token;
  
  case CHAR_RPAR: //)
    token = makeToken(SB_RPAR, lineNo, colNo);
    readChar();
    return token;
  
  case CHAR_SINGLEQUOTE:
    return readConstChar();
  
  case CHAR_DOUBLEQUOTE:
    return readString(); 
    // ....
  default:
    token = makeToken(TK_NONE, lineNo, colNo);
    error(ERR_INVALID_SYMBOL, lineNo, colNo);
    readChar(); 
    return token;
  }
}


Token* getValidToken(void) {
  Token *token = getToken();
  while (token->tokenType == TK_NONE) {
    free(token);
    token = getToken();
  }
  return token;
}


/******************************************************************/

void printToken(Token *token) {

  printf("%d-%d:", token->lineNo, token->colNo);

  switch (token->tokenType) {
  case TK_NONE: printf("TK_NONE\n"); break;
  case TK_IDENT: printf("TK_IDENT(%s)\n", token->string); break;
  case TK_NUMBER: printf("TK_NUMBER(%s)\n", token->string); break;
  case TK_CHAR: printf("TK_CHAR(\'%s\')\n", token->string); break;
  case TK_DOUBLE: printf("TK_DOUBLE(%s)\n", token->string); break; // (1)
  case TK_STRING: printf("TK_STRING(\"%s\")\n", token->string); break; // (1)
  case TK_EOF: printf("TK_EOF\n"); break;

  case KW_PROGRAM: printf("KW_PROGRAM\n"); break;
  case KW_CONST: printf("KW_CONST\n"); break;
  case KW_TYPE: printf("KW_TYPE\n"); break;
  case KW_VAR: printf("KW_VAR\n"); break;
  case KW_INTEGER: printf("KW_INTEGER\n"); break;
  case KW_CHAR: printf("KW_CHAR\n"); break;
  case KW_ARRAY: printf("KW_ARRAY\n"); break;
  case KW_OF: printf("KW_OF\n"); break;
  case KW_FUNCTION: printf("KW_FUNCTION\n"); break;
  case KW_PROCEDURE: printf("KW_PROCEDURE\n"); break;
  case KW_BEGIN: printf("KW_BEGIN\n"); break;
  case KW_END: printf("KW_END\n"); break;
  case KW_CALL: printf("KW_CALL\n"); break;
  case KW_IF: printf("KW_IF\n"); break;
  case KW_THEN: printf("KW_THEN\n"); break;
  case KW_ELSE: printf("KW_ELSE\n"); break;
  case KW_WHILE: printf("KW_WHILE\n"); break;
  case KW_DO: printf("KW_DO\n"); break;
  case KW_FOR: printf("KW_FOR\n"); break;
  case KW_TO: printf("KW_TO\n"); break;
  case KW_DOUBLE: printf("KW_DOUBLE\n"); break; // (1)
  case KW_STRING: printf("KW_STRING\n"); break; // (1)

  case SB_SEMICOLON: printf("SB_SEMICOLON\n"); break;
  case SB_COLON: printf("SB_COLON\n"); break;
  case SB_PERIOD: printf("SB_PERIOD\n"); break;
  case SB_COMMA: printf("SB_COMMA\n"); break;
  case SB_ASSIGN: printf("SB_ASSIGN\n"); break;
  case SB_EQ: printf("SB_EQ\n"); break;
  case SB_NEQ: printf("SB_NEQ\n"); break;
  case SB_LT: printf("SB_LT\n"); break;
  case SB_LE: printf("SB_LE\n"); break;
  case SB_GT: printf("SB_GT\n"); break;
  case SB_GE: printf("SB_GE\n"); break;
  case SB_PLUS: printf("SB_PLUS\n"); break;
  case SB_MINUS: printf("SB_MINUS\n"); break;
  case SB_TIMES: printf("SB_TIMES\n"); break;
  case SB_SLASH: printf("SB_SLASH\n"); break;
  case SB_LPAR: printf("SB_LPAR\n"); break;
  case SB_RPAR: printf("SB_RPAR\n"); break;
  case SB_ASSIGN_PLUS: printf("SB_ASSIGN_PLUS\n"); break;
  case SB_ASSIGN_SUBTRACT: printf("SB_ASSIGN_SUBTRACT\n"); break;
  case SB_ASSIGN_TIMES: printf("SB_ASSIGN_TIMES\n"); break;
  case SB_ASSIGN_DIVIDE: printf("SB_ASSIGN_DIVIDE\n"); break;
  case SB_OPEN_BRACKET: printf("SB_OPEN_BRACKET\n"); break;
  case SB_CLOSE_BRACKET: printf("SB_CLOSE_BRACKET\n"); break;
  case SB_MODULUS: printf("SB_MODULUS\n"); break;
  }
}

