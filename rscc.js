//----------------LEXER, PARSER AND AST------------------
//Reads input stream character by character
{
  function IStream(code) {
    let pos = 0,
      col = 0,
      line = 1;
    return {
      next: () => {
        let ch = code.charAt(pos++);
        if (ch == "\n") line++, (col = 0);
        else col++;
        return ch;
      },
      peek: () => {
        return code.charAt(pos);
      },
      eof: () => {
        return code.charAt(pos) == "";
      },
      printErr: msg => {
        throw new Error(msg + " (" + line + ":" + col + ")");
      }
    };
  }

  //Lexer(or tokenenizer)
  function Lexer(input) {
    //Current tokenen
    let curr = null;
    //Keywords of the language
    let kwords = " if then else fun true false loop stuff unstuff ";
    return {
      next: () => {
        let token = curr;
        curr = null;
        return token || read_next();
      },
      peek: () => {
        return curr || (curr = read_next());
      },
      eof: () => {
        return (curr || (curr = read_next())) == null;
      },
      printErr: input.printErr
    };
    //Reads stream while predicate is true
    function read_while(predicate) {
      let str = "";
      while (!input.eof() && predicate(input.peek())) str += input.next();
      return str;
    }
    //Proccess next tokenen
    function read_next() {
      read_while(ch => {
        return " \t\n\r".indexOf(ch) >= 0;
      });
      if (input.eof()) return null;
      let ch = input.peek();
      if (ch == "~") {
        read_while(ch => {
          return ch != "\n";
        });
        input.next();
        return read_next();
      }
      if (ch == '"') {
        let escaped = false,
          str = "";
        input.next();
        while (!input.eof()) {
          let ch = input.next();
          if (escaped) {
            str += ch;
            escaped = false;
          } else if (ch == "\\") {
            escaped = true;
          } else if (ch == '"') {
            break;
          } else {
            str += ch;
          }
        }
        return { type: "str", value: str };
      }
      if (/[0-9]/i.test(ch)) {
        let has_dot = false;
        let number = read_while(ch => {
          if (ch == ".") {
            if (has_dot) return false;
            has_dot = true;
            return true;
          }
          return /[0-9]/i.test(ch);
        });
        return { type: "num", value: parseFloat(number) };
      }
      if (/[a-zλ_]/i.test(ch)) {
        let id = read_while(ch => {
          return /[a-zλ_]/i.test(ch) || "-0123456789".indexOf(ch) >= 0;
        });
        return {
          type: kwords.indexOf(" " + id + " ") >= 0 ? "kw" : "let",
          value: id
        };
      }
      if (",;()[]".indexOf(ch) >= 0)
        return {
          type: "punc",
          value: input.next()
        };
      function isOperand_char(ch) {
        return "+-*/%=&|<>!".indexOf(ch) >= 0;
      }
      if (isOperand_char(ch))
        return {
          type: "op",
          value: read_while(isOperand_char)
        };
      input.printErr("Can't handle character: " + ch);
    }
  }

  //Building AST
  function AST(input) {
    let PRECEDENCE = {
      "=": 1,
      "||": 2,
      "&&": 3,
      "<": 7,
      ">": 7,
      "<=": 7,
      ">=": 7,
      "==": 7,
      "!=": 7,
      "+": 10,
      "-": 10,
      "*": 20,
      "/": 20,
      "%": 20
    };
    //---utility funkcijos---
    function isPunctuation(ch) {
      let token = input.peek();
      return (
        token && token.type == "punc" && (!ch || token.value == ch) && token
      );
    }
    function isKeyword(kw) {
      let token = input.peek();
      return token && token.type == "kw" && (!kw || token.value == kw) && token;
    }
    function isOperand(op) {
      let token = input.peek();
      return token && token.type == "op" && (!op || token.value == op) && token;
    }
    function skipPunctuation(ch) {
      if (isPunctuation(ch)) input.next();
      else input.printErr('Expecting punctuation: "' + ch + '"');
    }
    function skipKeyword(kw) {
      if (isKeyword(kw)) input.next();
      else input.printErr('Expecting keyword: "' + kw + '"');
    }
    //--------Parsing functions---------
    function parseBinary(left, rPrec) {
      let token = isOperand();
      if (token) {
        let lPrec = PRECEDENCE[token.value];
        if (lPrec > rPrec) {
          input.next();
          return parseBinary(
            {
              type: token.value == "=" ? "assign" : "binary",
              operator: token.value,
              left: left,
              right: parseBinary(parse_Part(), lPrec)
            },
            rPrec
          );
        }
      }
      return left;
    }

    function callParse(func) {
      return {
        type: "call",
        func: func,
        args: delimit("(", ")", ",", exprParse)
      };
    }
    function argsParse() {
      let name = input.next();
      if (name.type != "let") input.printErr("Expecting argument name");
      return name.value;
    }
    function ifParse() {
      skipKeyword("if");
      let cond = exprParse();
      let then = exprParse();
      let res = {
        type: "if",
        cond: cond,
        then: then
      };
      if (isKeyword("else")) {
        input.next();
        res.else = exprParse();
      }
      return res;
    }
    function funParse() {
      return {
        type: "fun",
        args: delimit("[", "]", ",", argsParse),
        body: exprParse()
      };
    }
    function boolParse() {
      return {
        type: "bool",
        value: input.next().value == "true"
      };
    }
    function loopParse() {
      skipKeyword("loop");
      let cond = exprParse();
      let body = exprParse();
      let ret = {
        type: "loop",
        cond: cond,
        body: body
      };
      return ret;
    }
    function stuffParse() {
      skipKeyword("stuff");
      let base = exprParse();
      let stuff = exprParse();
      let ret = {
        type: "stuff",
        base: base,
        stuff: stuff
      };
      return ret;
    }
    function unstuffParse() {
      skipKeyword("unstuff");
      return {
        type: "unstuff",
        base: exprParse()
      };
    }
    function blockParse() {
      let prog = delimit("[", "]", ",", exprParse);
      return { type: "block", block: prog };
    }
    function exprParse() {
      return proccessCall(() => {
        return parseBinary(parse_Part(), 0);
      });
    }
    function parse_Part() {
      return proccessCall(() => {
        if (isPunctuation("(")) {
          input.next();
          let exp = exprParse();
          skipPunctuation(")");
          return exp;
        }
        if (isPunctuation("[")) return blockParse();
        if (isKeyword("if")) return ifParse();
        if (isKeyword("loop")) return loopParse();
        if (isKeyword("stuff")) return stuffParse();
        if (isKeyword("unstuff")) return unstuffParse();
        if (isKeyword("true") || isKeyword("false")) return boolParse();
        if (isKeyword("fun")) {
          input.next();
          return funParse();
        }
        if (isKeyword("ret")) {
          return retParse();
        }
        let token = input.next();
        if (token.type == "let" || token.type == "num" || token.type == "str")
          return token;
        input.printErr("Unexpected tokenen: " + JSON.stringify(input.peek()));
      });
    }
    //--------Main functions------
    function proccessCall(expr) {
      expr = expr();
      return isPunctuation("(") ? callParse(expr) : expr;
    }

    function delimit(start, stop, separator, parser) {
      let a = [],
        first = true;
      skipPunctuation(start);
      while (!input.eof()) {
        if (isPunctuation(stop)) break;
        if (first) first = false;
        else skipPunctuation(separator);
        if (isPunctuation(stop)) break;
        a.push(parser());
      }
      skipPunctuation(stop);
      return a;
    }

    function proccess() {
      let prog = [];
      while (!input.eof()) prog.push(exprParse());
      return { type: "block", block: prog };
    }
    return proccess();
  }
}
//----------------------------------------------
//----------------INTERPRETER-------------------
//Create seperate environment for main block and other block scopes
{
  //Main function
  function evaluate(exp, env) {
    switch (exp.type) {
      case "num":
      case "str":
      case "bool":
        return exp.value;
      case "let":
        return env.get(exp.value);
      case "assign":
        if (exp.left.type != "let")
          throw new Error("Failed assignment: " + JSON.stringify(exp.left));
        return env.set(exp.left.value, evaluate(exp.right, env));
      case "binary":
        return apply_op(
          exp.operator,
          evaluate(exp.left, env),
          evaluate(exp.right, env)
        );
      case "fun":
        return create_fun(env, exp);
      case "loop":
        //while loop
        if (exp.cond.block.length === 1) {
          let loopCond = evaluate(exp.cond, env);
          if (loopCond !== false) {
            evaluate(exp.body, env);
            return evaluate(exp, env);
          }
          return false;
          //for loop
        } else if (exp.cond.block.length === 3) {
          let scope = env;
          if (Object.keys(env.args).length != 1) scope = env.extend();
          if (!scope.exists(exp.cond.block[0].left.value))
            evaluate(exp.cond.block[0], scope);
          let forLoopCond = evaluate(exp.cond.block[1], scope);
          if (forLoopCond !== false) {
            evaluate(exp.body, scope);
            evaluate(exp.cond.block[2], scope);
            return evaluate(exp, scope);
          }
          return false;
        } else {
          throw new Error(
            "Unexpected arguments amount in loop condition. Expected 1 or 3, got: " +
              exp.cond.block.length
          );
        }
      case "if":
        let condition = evaluate(exp.cond, env);
        if (condition !== false) return evaluate(exp.then, env);
        return exp.else ? evaluate(exp.else, env) : false;
      case "stuff":
        //Doesnt set "stuff"
        evaluate(exp.base, env);
        return env.setStuff(exp.base.block[0].value, evaluate(exp.stuff, env));
      case "unstuff":
        evaluate(exp.base, env);
        return env.getStuff(exp.base.block[0].value);
      case "block":
        let value = false;
        exp.block.forEach(exp => {
          value = evaluate(exp, env);
        });
        return value;
      case "call":
        let func = evaluate(exp.func, env);
        return func.apply(
          null,
          exp.args.map(arg => {
            return evaluate(arg, env);
          })
        );
      default:
        throw new Error("Undefined evaluation: " + exp.type);
    }
    function apply_op(op, a, b) {
      function num(x) {
        if (typeof x != "number")
          throw new Error("Number was expected. Instead got: " + x);
        return x;
      }
      function div(x) {
        if (num(x) == 0)
          throw new Error("Unfortunately, division by zero is unsupported");
        return x;
      }
      switch (op) {
        case "+":
          return num(a.valueOf()) + num(b);
        case "-":
          return num(a.valueOf()) - num(b);
        case "*":
          return num(a.valueOf()) * num(b);
        case "/":
          return num(a.valueOf()) / div(b);
        case "%":
          return num(a.valueOf()) % div(b);
        case "&&":
          return a !== false && b;
        case "||":
          return a !== false ? a : b;
        case "<":
          return num(a) < num(b);
        case ">":
          return num(a) > num(b);
        case "<=":
          return num(a) <= num(b);
        case ">=":
          return num(a) >= num(b);
        case "==":
          return a === b;
        case "!=":
          return a !== b;
      }
      throw new Error("Error applying operator: " + op);
    }
    function create_fun(env, exp) {
      function fun() {
        let names = exp.args;
        let scope = env.extend();
        for (let i = 0; i < names.length; i++)
          scope.def(names[i], i < arguments.length ? arguments[i] : false);
        return evaluate(exp.body, scope);
      }
      return fun;
    }
  }
  //PropTypes initiliziation
  function Env(parent) {
    this.args = Object.create(parent ? parent.args : null);
    this.parent = parent;
  }
  Env.prototype = {
    //new block
    extend: function() {
      return new Env(this);
    },
    //current val of var
    get: function(name) {
      if (name in this.args) return this.args[name];
      throw new Error("Undefined var: " + name);
    },
    exists: function(name) {
      let a = Object.getOwnPropertyNames(this.args);
      return a.includes(name) ? true : false;
    },
    //set var val globally
    set: function(name, value) {
      return (this.args[name] = value);
    },
    setStuff: function(name, value) {
      this.args[name] = Object(this.args[name]);
      return (this.args[name].stuff = value);
    },
    getStuff: function(name) {
      if (name in this.args) return this.args[name].stuff;
      throw new Error("Undefined stuffed var: " + name);
    },
    //add var to local block
    def: function(name, value) {
      return (this.args[name] = value);
    }
  };
}
let interpreter = new Env();
//--------------System functions---------------
interpreter.def("print", string => {
  console.log(string);
});
interpreter.def("exp", (a, b) => {
  return Math.pow(a, b);
});
interpreter.def("fact", a => {
  let res = 1;
  for (let i = 1; i <= a; ++i) res *= i;
  return res;
});
//------------------Read file-------------------
const fs = require("fs");
const code = fs.readFileSync(process.argv[2], "utf-8");
//------------------Interpret-------------------
console.log("==============INTERPRETER STARTED===========");
console.log("=========INTERPRETING: " + process.argv[2] + "=========");
let ast = AST(Lexer(IStream(code)));
evaluate(ast, interpreter);
console.log("==============SUCCESSFUL=====================");
