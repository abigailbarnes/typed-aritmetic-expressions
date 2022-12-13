// HW3: CMSC 22100, Autumn 2022

// === data definitions

enum Token:
  case LParen
  case RParen
  case KW_true
  case KW_false
  case Z // the digit 0
  case KW_if
  case KW_succ
  case KW_pred
  case KW_iszero
  case KW_and
  case KW_or
  case KW_not
  case KW_pair
  case KW_first
  case KW_second 
  case GT // the symbol >
  case KW_eq

enum Term:
  case True
  case False
  case Zero
  case If(t1:Term,t2:Term,t3:Term)
  case Succ(t1:Term)
  case Pred(t1:Term)
  case IsZero(t1:Term)
  case And(t1:Term,t2:Term)
  case Or(t1:Term,t2:Term)
  case Not(t1:Term)
  case Pair(t1:Term,t2:Term)
  case First(t1:Term)
  case Second(t1:Term)
  case GreaterThan(t1:Term,t2:Term)
  case Eq(t1:Term,t2:Term)

enum Type:
  case Bool
  case Nat
  case Cross(tau1:Type,tau2:Type)

enum NormalForm:
  case Value(t:Term)
  case Stuck(t:Term)

// utilities

// === value judgments

def isV(t: Term): Boolean = t match
    case Term.Zero => true
    case Term.True => true
    case Term.False => true
    case _ => false

def isNV(t: Term): Boolean = t match
    case Term.Zero => true
    case Term.Succ(v) => true
    case _ => false

// === lexical scanner

//def nextToken(cs: List[Char]): Option[(Token, List[Char])] = throw new Exception("todo: nextToken")
def nextToken(cs: List[Char]): Option[(Token, List[Char])] = 
    if(cs.isEmpty)
        return None
    else
        var tail = cs.tail
        var head = cs.head

        if(Character.isWhitespace(head))
            nextToken(tail)
        else
            cs match
                case Nil => None
                case '(' :: tail => Some(Token.LParen, tail)
                case ')' :: tail => Some(Token.RParen, tail)
                case 't' :: 'r' :: 'u' :: 'e' :: tail => Some(Token.KW_true, tail)
                case 'f' :: 'a' :: 'l' :: 's' :: 'e' :: tail => Some(Token.KW_false, tail)
                case '0' :: tail => Some(Token.Z, tail)
                case 'i' :: 'f' :: tail => Some(Token.KW_if, tail)
                case 's' :: 'u' :: 'c' :: 'c' :: tail => Some(Token.KW_succ, tail)
                case 'p' :: 'r' :: 'e' :: 'd' :: tail => Some(Token.KW_pred, tail)
                case 'i' :: 's' :: 'z' :: 'e' :: 'r' :: 'o' :: tail => Some(Token.KW_iszero, tail)
                case 'a' :: 'n' :: 'd' :: tail => Some(Token.KW_and, tail)
                case 'o' :: 'r' :: tail => Some(Token.KW_or, tail)
                case 'n' :: 'o' :: 't' :: tail => Some(Token.KW_not, tail)
                case 'p' :: 'a' :: 'i' :: 'r' :: tail => Some(Token.KW_pair, tail)
                case 'f' :: 'i' :: 'r' :: 's' :: 't' :: tail => Some(Token.KW_first, tail)
                case 's' :: 'e' :: 'c' :: 'o' :: 'n' :: 'd' :: tail => Some(Token.KW_second, tail)
                case '>' :: tail => Some(Token.GT, tail)
                case '=' :: tail => Some(Token.KW_eq, tail)
                //case Character.isWhitespace(head) :: tail => nextToken(tail) //skipping white space
                case _ => throw new Exception("Token Error: input cannot be tokenized")
    
def scan(code: String): List[Token] = 
  def lp(cs: List[Char]): List[Token] = nextToken(cs) match {
    case None => Nil
    case Some(tok,tl) => tok::lp(tl)}
  return lp(code.toList)

// === parser

//def nextTerm(ts: List[Token]): Option[(Term, List[Token])] = throw new Exception("todo: nextTerm")
def nextTerm(ts: List[Token]): Option[(Term, List[Token])] =
    if(ts.isEmpty)
        return None
    else
        var tail = ts.tail
        var head = ts.head// remove before this
        ts match
            case Nil => None
            case Token.KW_true :: tail => Some(Term.True, tail)
            case Token.KW_false :: tail => Some(Term.False, tail)
            case Token.Z :: tail => Some(Term.Zero, tail)
            //(succ t)
            case Token.LParen :: Token.KW_succ :: tail =>
                nextTerm(tail) match
                    case None => throw new Exception("nextTerm Error: succ ended unexpectedly")
                    //we are type checking tail1 to be a right parenthesis
                    case Some(t1, Token.RParen :: tail1) => Some(Term.Succ(t1), tail1)
                    case _ => throw new Exception("nextTerm Error: succ missing right parenthesis")
            //(pred t)
            case Token.LParen :: Token.KW_pred :: tail =>
                nextTerm(tail) match
                    case None => throw new Exception("nextTerm Error: pred ended unexpectedly")
                    case Some(t1, Token.RParen :: tail1) => Some(Term.Pred(t1), tail1)
                    case _ => throw new Exception("nextTerm Error: pred missing right parenthesis")
            //(iszero t)
            case Token.LParen :: Token.KW_iszero :: tail => 
                nextTerm(tail) match
                    case None => throw new Exception("nextTerm Error: iszero ended unexpectedly")
                    case Some(t1, Token.RParen :: tail1) => Some(Term.IsZero(t1), tail1)
                    case _ => throw new Exception("nextTerm Error: iszero missing right parenthesis")
            //(and t t)
            case Token.LParen :: Token.KW_and :: tail =>
                nextTerm(tail) match
                    case None => throw new Exception("nextTerm Error: and ended unexpectedly")
                    case Some(t1, tail1) =>
                        nextTerm(tail1) match
                            case Some(t2, t3::tail2) => t3 match
                                case Token.RParen => Some(Term.And(t1, t2), tail2)
                                case _ => throw new Exception("nextTerm Error: and missing right parenthesis")
                            case None => throw new Exception("nextTerm Error: and missing a term")
                            case _ => throw new Exception("nextTerm Error: and missing a term")
                            //case Some(t2, Token.RParen :: tail2) => Some(Term.And(t1, t2), tail2)
                            //case _ => throw new Exception("nextTerm Error: and ended unexpectedly")
            //(or t t)
            case Token.LParen :: Token.KW_or :: tail =>
                nextTerm(tail) match
                    case None => throw new Exception("nextTerm Error: or ended unexpectedly")
                    case Some(t1, tail1) =>
                        nextTerm(tail1) match
                            case Some(t2, t3::tail2) => t3 match
                                case Token.RParen => Some(Term.Or(t1, t2), tail2)
                                case _ => throw new Exception("nextTerm Error: or missing right parenthesis")
                            case None => throw new Exception("nextTerm Error: or missing a term")
                            case _ => throw new Exception("nextTerm Error: or missing a term")
            //(if t t t)
            case Token.LParen :: Token.KW_if :: tail =>
                nextTerm(tail) match
                    case Some(t1, tail1) => nextTerm(tail1) match
                        case Some(t2, tail2) => nextTerm(tail2) match
                            case Some(t3, t4 :: tail3) => t4 match
                                case Token.RParen => Some(Term.If(t1, t2, t3), tail3)
                                case _ => None
                            case _ => None
                        case None => None
                    case None => None
            //(not t)
            case Token.LParen :: Token.KW_not :: tail =>
              nextTerm(tail) match 
                case None => None
                case Some(t1, Token.RParen :: tail1) => Some(Term.Not(t1), tail1)
                case _ => None
            //(pair t t)
            case Token.LParen :: Token.KW_pair :: tail =>
                nextTerm(tail) match
                    case None => None
                    case Some(t1, tail1) =>
                        nextTerm(tail1) match
                            case Some(t2, t3::tail2) => t3 match
                                case Token.RParen => Some(Term.Pair(t1, t2), tail2)
                                case _ => None
                            case None => None
                            case _ => None
            //(first t)
            case Token.LParen :: Token.KW_first :: tail =>
              nextTerm(tail) match 
                case None => None
                case Some(t1, Token.RParen :: tail1) => Some(Term.First(t1), tail1)
                case _ => None
            //(second t)
            case Token.LParen :: Token.KW_second :: tail =>
              nextTerm(tail) match 
                case None => None
                case Some(t1, Token.RParen :: tail1) => Some(Term.Second(t1), tail1)
                case _ => None
            //(> t t)
            case Token.LParen :: Token.GT :: tail =>
                nextTerm(tail) match
                    case None => None
                    case Some(t1, tail1) =>
                        nextTerm(tail1) match
                            case Some(t2, t3::tail2) => t3 match
                                case Token.RParen => Some(Term.GreaterThan(t1, t2), tail2)
                                case _ => None
                            case None => None
                            case _ => None
            //(= t t)
            case Token.LParen :: Token.KW_eq :: tail =>
                nextTerm(tail) match
                    case None => None
                    case Some(t1, tail1) =>
                        nextTerm(tail1) match
                            case Some(t2, t3::tail2) => t3 match
                                case Token.RParen => Some(Term.Eq(t1, t2), tail2)
                                case _ => None
                            case None => None
                            case _ => None
            case _ => throw new Exception("not here yet")

def parse(tokens: List[Token]): Term =
  def lp(toks: List[Token]): Term =
    nextTerm(toks) match {
      case None => throw new Exception("not enough program")
      case Some(tm, Nil) => tm
      case Some(_) => throw new Exception("too much program")}
  return lp(tokens)

// === type checker


/*enum Type:
  case Bool
  case Nat
  case Cross(tau1:Type,tau2:Type)*/


// identify the type of the term t, if it is well-typed
// throw an exception if it is not
//def typeOf(t: Term): Type = throw new Exception("todo: typeOf")
def typeOf(t: Term): Type = 
  t match
    //true
    case Term.True => Type.Bool
    //false
    case Term.False => Type.Bool
    //zero
    case Term.Zero => Type.Nat
    //if t t t
    case Term.If(t1, t2, t3) if (typeOf(t1) == Type.Bool) && (typeOf(t2) == typeOf(t3)) => Type.Bool
    //succ t
    case Term.Succ(t1) if isNV(t1) => typeOf(t1)
    //pred t
    case Term.Pred(t1) if isNV(t1) => typeOf(t1)
    //iszero t
    case Term.IsZero(t1) if isNV(t1) => Type.Bool
    //and t t
    case Term.And(t1, t2) if (typeOf(t1) == typeOf(t2)) => Type.Bool
    //or t t
    case Term.Or(t1, t2) if (typeOf(t1) == typeOf(t2)) => Type.Bool
    //not t
    case Term.Not(t1) if (typeOf(t1) == Type.Bool) => Type.Bool
    //pair t t
    case Term.Pair(t1, t2) => Type.Cross(typeOf(t1), typeOf(t2))
    //first (pair t t)
    case Term.First(Term.Pair(t1, t2)) => typeOf(t1)
    //second (pair t t )
    case Term.Second(Term.Pair(t1, t2)) => typeOf(t2)
    //> t t
    case Term.GreaterThan(t1, t2) if (typeOf(t1) == typeOf(t2)) && isNV(t1) && isNV(t2) => Type.Bool
    //= t t
    case Term.Eq(t1, t2) if (typeOf(t1) == typeOf(t2)) => Type.Bool
    //wildcard: catch all
    case _ => throw new Exception("Term is ill-typed")


// === small-step evaluator

//def step(t: Term): Option[Term] = throw new Exception("todo: step")
def step(t: Term): Option[Term] = 
  if(isV(t))
        return None
  else
      t match
          //Succ(t1)
          case Term.Succ(t1) => 
              if(isNV(t1))
                  None
              else
                  step(t1) match
                      case Some(u) => Some(Term.Succ(u))
                      case _ => None
          
          //Pred(t1)
          //                                                              Office Hours for Pred!!!!
          case Term.Pred(t1) => t1 match
              case Term.Zero => Some(Term.Zero)
              //case Some(u) => Some(Term.Succ(u))
              case _ => None
          
          //isZero(t1)
          case Term.IsZero(t1) => t1 match 
              case Term.Zero => Some(Term.True)
              case t if isNV(t) => Some(Term.False)
              case t1 => step(t1) match
                  case Some(t1) => Some(Term.IsZero(t1))
                  case None => None
                      
          //And(t1, t2)
          case Term.And(t1, t2) => t1 match
              //if the first term is True
              case Term.True => Some(t2)
              //if the first term is False
              case Term.False => Some(Term.False)
              case t1 => step(t1) match  
                  //t3 is functionally t1' 
                  case Some(t3) => Some(Term.And(t3, t2))
                  case _ => None

          //Or(t1, t2)
          case Term.Or(t1, t2) =>  t1 match
              case Term.True => Some(Term.True)
              case Term.False => Some(t2)
              case t1 => step(t1) match
                  //t3 functionally acts as t1'
                  case Some(t3) => Some(Term.Or(t3, t2))
                  case None => None

          //If(t1, t2, t3)
          case Term.If(t1, t2, t3) => t1 match
              case Term.True => Some(t2)
              case Term.False => Some(t3)
              case t1 => step(t1) match
                  //t4 is functionally t1'
                  case Some(t4) => Some(Term.If(t4, t2, t3))
                  case None => None

          //Not(t1)
          case Term.Not(t1) => t1 match
            case Term.True => Some(Term.False)
            case Term.False => Some(Term.True)
            case t1 => step(t1) match
              //t4 is functionally t1'
              case Some(t4) => Some(Term.Not(t4))
              case None => None

          //                                  Want to check in OHs after this point
          //Pair(t1, t2)
          case Term.Pair(t1, t2) => 
            if(isV(t1) && isV(t2))
              None
            else if(isV(t1))
              step(t2) match
                case Some(t3) => Some(Term.Pair(t1, t3))
                case None => None
            else if(isV(t2))
              step(t1) match
                case Some(t3) => Some(Term.Pair(t3, t2))
                case None => None
            else
              None

          //First(t1)
          case Term.First(t1) => t1 match
            case Term.Pair(t2, t3) =>
              if(isV(t2))
                Some(t2)
              else
                step(t2) match
                  case Some(t4) => Some(Term.First(Term.Pair(t4, t3)))
                  case None => None
            case t1 if isV(t1) =>
              None
            case t1 =>
              step(t1) match
                case Some(t4) => Some(Term.First(t4))
                case None => None

          //Second(t1)
          case Term.Second(t1) => t1 match
            case Term.Pair(t2, t3) =>
              if(isV(t3))
                Some(t3)
              else
                step(t3) match
                  case Some(t4) => Some(Term.Second(Term.Pair(t2, t4)))
                  case None => None
            case t1 if isV(t1) =>
              None
            case t1 =>
              step(t1) match
                case Some(t4) => Some(Term.Second(t4))
                case None => None

          //> t t
          case Term.GreaterThan(Term.Zero, t2) => 
            if(isNV(t2))
              Some(Term.False)
            else  
              None
          case Term.GreaterThan(t1, Term.Zero) =>
            if(isNV(t1))
              Some(Term.True)
            else
              None
          case Term.GreaterThan(Term.Succ(t1), Term.Succ(t2)) =>
            Some(Term.GreaterThan(t1, t2))

          //= t t
          case Term.Eq(Term.True, Term.True) =>  
            Some(Term.True)
          case Term.Eq(Term.False, Term.False) =>
            Some(Term.True)
          case Term.Eq(Term.True, Term.False) =>
            Some(Term.False)
          case Term.Eq(Term.False, Term.True) =>
            Some(Term.False)
          case Term.Eq(Term.Zero, Term.Zero) =>
            Some(Term.True)
          case Term.Eq(Term.Zero, Term.Succ(t2)) =>
            Some(Term.False)
          case Term.Eq(Term.Succ(t1), Term.Zero) =>
            Some(Term.False)
          case Term.Eq(Term.Succ(t1), Term.Succ(t2)) =>
            Some(Term.Eq(t1, t2))
          case Term.Eq(Term.Pair(t1, t2), Term.Pair(t3, t4)) =>
            Some(Term.And(Term.Eq(t1, t2), Term.Eq(t3, t4)))
          case Term.Eq(t1, t2)=>
            if(isV(t1) && isV(t2))
              Some(Term.Eq(t1, t2))
            else if(isV(t1))
              step(t2) match
                case Some(t3) => Some(Term.Eq(t1, t3))
                case None => None
            else if(isV(t2))
              step(t1) match
                case Some(t3) => Some(Term.Eq(t3, t2))
                case None => None
            else  
              None
              
          //wildcard
          case _ => None

def steps(t: Term): List[Term] = step(t) match {
  case None => t::Nil
  case Some(u) => t::steps(u)}

// === interpreter

def classify(t: Term): NormalForm = step(t) match {
  case None => if isV(t) then NormalForm.Value(t) else NormalForm.Stuck(t)
  case _ => throw new Exception("classify given non-normal form: " + t)}

@main def interpret(code: String): Unit =
  val tokens = scan(code)
  val ast    = parse(tokens)
  val terms  = steps(ast)
  try
    println("type: " + typeOf(ast))
  catch
    case e => println("type error: " + e)
  println()
  println("     " + terms.head)
  for t <- terms.tail
  do
    println("---> " + t)
  println("-/->")
  println()
  println(classify(terms.last))



@main def test_nextToken() = 
  val not = List('n', 'o', 't', '(', ')')
  printf(s"testing nextToken with not: ${nextToken(not)}\n")

  val pair = List('p', 'a', 'i', 'r', '(', ')')
  printf(s"testing nextToken with pair: ${nextToken(pair)}\n")

  val first = List('f', 'i', 'r', 's', 't', '(', ')')
  printf(s"testing nextToken with first: ${nextToken(first)}\n")

  val second = List('s', 'e', 'c', 'o', 'n', 'd', '(', ')')
  printf(s"testing nextToken with second: ${nextToken(second)}\n")

  val greaterthan = List('>', '(', ')')
  printf(s"testing nextToken with greaterthan: ${nextToken(greaterthan)}\n")

  val equal = List('=', '(', ')')
  printf(s"testing nextToken with equal: ${nextToken(equal)}\n")

@main def test_nextTerm() =     
    printf(s"testing nextTerm with List(LParen,Not,True,RParen): ${nextTerm(List(Token.LParen,Token.KW_not,Token.KW_true,Token.RParen))}\n")
    printf(s"testing nextTerm with List(LParen,Pair,Zero,True,RParen): ${nextTerm(List(Token.LParen,Token.KW_pair,Token.Z,Token.KW_true,Token.RParen))}\n")
    printf(s"testing nextTerm with List(LParen,First,Zero,RParen): ${nextTerm(List(Token.LParen,Token.KW_first,Token.Z,Token.RParen))}\n")
    printf(s"testing nextTerm with List(LParen,Second,True,RParen: ${nextTerm(List(Token.LParen,Token.KW_second,Token.KW_true,Token.RParen))}\n")
    printf(s"testing nextTerm with List(LParen,GreaterThan,Zero,Zero,RParen: ${nextTerm(List(Token.LParen,Token.GT,Token.Z,Token.Z,Token.RParen))}\n")
    printf(s"testing nextTerm with List(LParen,Equal,True,True,RParen: ${nextTerm(List(Token.LParen,Token.KW_eq,Token.KW_true,Token.KW_true,Token.RParen))}\n")

@main def test_typeOf() = 
  printf(s"testing typeOf with True: ${typeOf(Term.True)}\n")
  printf(s"testing typeOf with False: ${typeOf(Term.False)}\n")
  printf(s"testing typeOf with Zero: ${typeOf(Term.Zero)}\n")
  //if t t t
  printf(s"testing typeOf with If: ${typeOf(Term.If(Term.True, Term.Zero, Term.Succ(Term.Zero)))}\n")
  //succ t
  printf(s"testing typeOf with Succ: ${typeOf(Term.Succ(Term.Zero))}\n")
  //pred t
  printf(s"testing typeOf with Pred: ${typeOf(Term.Pred(Term.Zero))}\n")
  //iszero t
  printf(s"testing typeOf with IsZero: ${typeOf(Term.IsZero(Term.Zero))}\n")
  //and t t
  printf(s"testing typeOf with And: ${typeOf(Term.And(Term.True, Term.False))}\n")
  //or t t
  printf(s"testing typeOf with Or: ${typeOf(Term.Or(Term.True, Term.False))}\n")
  //not t
  printf(s"testing typeOf with Not: ${typeOf(Term.Not(Term.False))}\n")
  //pair t t
  printf(s"testing typeOf with Pair: ${typeOf(Term.Pair(Term.Zero, Term.True))}\n")
  //first (pair t t)
  printf(s"testing typeOf with First: ${typeOf(Term.First(Term.Pair(Term.Zero, Term.True)))}\n")
  //second (pair t t)
  printf(s"testing typeOf with Second: ${typeOf(Term.Second(Term.Pair(Term.Zero, Term.True)))}\n")
  //> t t
  printf(s"testing typeOf with >: ${typeOf(Term.GreaterThan(Term.Zero, Term.Succ(Term.Zero)))}\n")
  //= t t
  printf(s"testing typeOf with =: ${typeOf(Term.Eq(Term.Zero, Term.Zero))}\n")
  //wildcard: ill typed
  /*printf(s"testing typeOf with Ill-Typed: ${typeOf(Term.If(Term.Zero, Term.True, Term.False))}\n")
  printf(s"testing typeOf with Ill-Typed: ${typeOf(Term.Succ(Term.True))}\n")
  printf(s"testing typeOf with Ill-Typed: ${typeOf(Term.First(Term.Zero))}\n")*/


@main def test_step() = 
  print(s"testing step with (= 0 0): ${step(Term.Eq(Term.Zero,Term.Zero))}\n")
  print(s"testing step with (pair 0 true): ${step(Term.Pair(Term.Zero,Term.True))}\n")
  //print(s"testing step with (= 0 0): ${step(Term.Eq(Term.Zero,Term.Zero))}")
  print(s"testing step with (if f 0 (= 0 0)): ${step(Term.If(Term.False, Term.Zero, Term.Eq(Term.Zero, Term.Zero)))}\n")
  print(s"testing step with (if (= 0 0) t f): ${step(Term.If(Term.Eq(Term.Zero, Term.Zero), Term.True, Term.False))}\n")

/*@main def test_interpret() = 
  print(s"testing step with (pair true 0): ${interpret("(pair true 0)")}\n")*/
