#lang scribble/text

@(require (planet samsergey/rewrite:1:0))

@(require racket/path
          racket/string
          racket/dict
          "../misc/c-pre.rkt"
          "c-core.rkt")

@(provide type-make
          type-read
          extern-get/set
          scheme-get/set
          safe-conversion-table)



@; ┏━┓┏━╸╻ ╻┏━┓╻╺┳╸┏━╸   ┏━┓╻ ╻╻  ┏━╸┏━┓
@; ┣┳┛┣╸ ┃╻┃┣┳┛┃ ┃ ┣╸ ╺━╸┣┳┛┃ ┃┃  ┣╸ ┗━┓
@; ╹┗╸┗━╸┗┻┛╹┗╸╹ ╹ ┗━╸   ╹┗╸┗━┛┗━╸┗━╸┗━┛
 
@(define (type-read x)
   (define rules
    (replace-all
     (list `extern-type _)
       --> (format "s7_object_value_s(~~a, _~a_type_id, ~~a)" (c-type x))
     
     (list `new-type _)
       --> (format "s7_object_value_s(~~a, _~a_type_id, ~~a)" (c-type x))
     
     (list `primitive-type _)
       --> (format "s7_~a_s(~~a, ~~a)" (c-type x))))
   
   (rules x))

@; TODO: does s_make_string require a free()?
@(define (type-make x)
   (define rules
    (replace-all
     (list `extern-type _)
       --> (format "s7_make_object(~~a, _~a_type_id, ~~a)" (c-type x))
     
     (list `new-type _)
       --> (format "s7_make_object(~~a, _~a_type_id, ~~a)" (c-type x))
     
     (list `primitive-type _)
       --> (format "s7_make_~a(~~a, ~~a)" (c-type x))))

   (rules x))




@; ╻ ╻┏━┓┏━┓   ┏━╸┏━╸╺┳╸ ╻┏━┓┏━╸╺┳╸
@; ┃┏┛┣━┫┣┳┛╺━╸┃╺┓┣╸  ┃ ┏┛┗━┓┣╸  ┃ 
@; ┗┛ ╹ ╹╹┗╸   ┗━┛┗━╸ ╹ ╹ ┗━┛┗━╸ ╹ 
@; generated get/set ops for each type

@(define (scheme-set-decl x)
  (format "int ss_set_~a(scheme s, const char *name, ~a v)"
          (c-type x)
          (c-type x)))
  
@(define (scheme-get-decl x)
  (format "int ss_get_~a(scheme s, const char *name, ~a *result)"
          (c-type x)
          (c-type x)))


@(define (extern-get/set x)
  @C{
    extern @|(scheme-set-decl x)|;
    extern @|(scheme-get-decl x)|;
    
    })

@(define (scheme-get/set x)
  (define get
      (format (type-read x) "s.s7" "s7_symbol_value(s.s7, sym)"))
  
  @C{
    /////// @|(c-type x)| ///////
    @(scheme-get-decl x)
    {
        s7_pointer sym = s7_symbol_table_find_name(s.s7, name);
        if (sym) {
            *result = (@|(c-type x)|) @|get|;
            return 0;
        } else {
            printf("ss_get_@|(c-type x)|: warning: symbol '%s' not found.\n",
                   name);
            return -1;
        }    
    }
        
    @(scheme-set-decl x)
    {
        s7_pointer sym = s7_symbol_table_find_name(s.s7, name);
        if (sym) {
            s7_symbol_set_value(s.s7, sym,
                                @| (format (type-make x) "s.s7" "v") |);
            return 0;
        } else {
            printf("ss_set_@|(c-type x)|: warning: symbol '%s' not found.\n",
                   name);
            return -1;
        }    
    }
            
  })


@(define safe-conversion-table
 ;|    name        |      test         |    convert
 ;|   invalid      |    result-type    |

 `(("s7_ulong_s"    "s7_is_ulong(p)"   "s7_ulong(p)"
    "ULONG_MAX"     "unsigned long")
   
   ("s7_integer_s"  "s7_is_integer(p)" "s7_integer(p)"
    "INT_MAX"       "s7_Int")
   
   ("s7_boolean_s"  "s7_is_boolean(p)" "s7_boolean(sc, p)"
    "false"         "bool")
   
   ("s7_real_s"     "s7_is_number(p)"  "s7_number_to_real(sc, p)"
    "NAN"           "s7_Double")
   
   ("s7_string_s"   "s7_is_string(p)"  "s7_string(p)"
    "NULL"          "const char *")))

