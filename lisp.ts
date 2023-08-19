const lisp = (src: string, onlyParse: boolean = false) => {
  type QuotePart =
    | {
        type: "string";
        value: string;
      }
    | { type: "tag"; value: string };
  type LispValue =
    | {
        type: "null";
        value: null;
      }
    | {
        type: "str";
        value: string;
      }
    | {
        type: "num";
        value: number;
      }
    | {
        type: "sym";
        value: string;
      }
    | {
        type: "symp";
        value: string;
      }
    | {
        type: "lambda";
        value: Lambda;
      }
    | {
        type: "builtin";
        value: string;
      }
    | {
        type: "plist";
        value: Plist;
      }
    | { type: "list"; value: Array<LispValue> };
  type List = Array<LispValue>;
  type Plist = Record<string, LispValue>;
  type Lambda = {
    containsSpreadArg: boolean;
    args: string[];
    code: AstExpr;
  };
  type AstExpr =
    | {
        type: "str";
        value: [string];
      }
    | {
        type: "num";
        value: [number];
      }
    | App
    | MacroApp
    | {
        type: "sym";
        value: [string];
      }
    | { type: "quotevar"; value: [string] }
    | {
        type: "quotevarinterpolated";
        value: Array<QuotePart>;
        original: "sym" | "symp";
      }
    | {
        type: "symp";
        value: [string];
      };
  type MacroApp = {
    type: "macroapp";
    value: Array<AstExpr>;
  };
  type App = {
    type: "app";
    value: Array<AstExpr>;
  };
  const makeStr = (value: string): AstExpr => ({
    type: "str",
    value: [value],
  });
  const makeNum = (value: number): AstExpr => ({
    type: "num",
    value: [value],
  });
  const makeQuotevar = (value: string): AstExpr => ({
    type: "quotevar",
    value: [value],
  });
  const makeSymp = (value: string): AstExpr => ({
    type: "symp",
    value: [value],
  });
  const makeSym = (value: string): AstExpr => {
    return {
      type: "sym",
      value: [value],
    };
  };
  const makeApply = (value: Array<AstExpr>): AstExpr => ({
    type: "app",
    value,
  });
  const parse = (src: string): Array<AstExpr> => {
    let idx = 0;
    const peek = (): string | undefined =>
      idx >= src.length ? undefined : src[idx];
    const matchEat = (expected: string): boolean | undefined => {
      const char = peek();
      if (char === undefined) return undefined;
      const match = char === expected;
      if (match) {
        idx += 1;
      }
      return match;
    };
    const AstExprs: Array<AstExpr> = [];
    function parseAstExpr(): AstExpr | undefined {
      eatWhitspace();
      const p = peek();
      if (!p) return undefined;
      switch (p) {
        case "(": {
          idx += 1;
          const app = parseApply();
          if (app !== undefined) {
            const first = (app as App).value[0];
            if (first.type === "sym") {
              const last = Math.max(0, first.value[0].length - 1);
              if (first.value[0][last] === "!") {
                if (first.value[0].length === 1)
                  throw new Error("Invalid macro call");
                first.value[0] = first.value[0].slice(0, last);
                return {
                  type: "macroapp",
                  value: (app as App).value,
                };
              }
            }
          }
          return app;
        }
        default: {
          const num = parseNumber(p);
          if (num !== undefined) {
            return makeNum(num);
          }
          const str = parseString(p);
          if (str !== undefined) return makeStr(str);
          const sym = parseSym(p);
          if (sym !== undefined) {
            if (sym[0] === "#") {
              const actualSym = sym.slice(1);
              if (actualSym.length === 0)
                throw new Error("Invalid empty symbol");
              return makeQuotevar(actualSym);
            }
            if (sym[0] === ":" || sym[0] === "'") {
              const actualSym = sym.slice(1);
              if (actualSym.length === 0)
                throw new Error("Invalid empty symbol");
              if (actualSym.includes("$")) {
                const quote = parseQuotedSym(actualSym, "symp");
                if (quote) return quote;
              }
              return makeSymp(actualSym);
            }
            if (sym.includes("$")) {
              const quote = parseQuotedSym(sym, "sym");
              if (quote) return quote;
            }
            return makeSym(sym);
          }
          throw new Error("Bad value");
        }
      }
    }
    function parseQuotedSym(
      sym: string,
      original: "sym" | "symp"
    ): AstExpr | undefined {
      const parts: QuotePart[] = [];

      // fuck#[nice]dude#[]
      let tagStart: number | undefined = undefined;
      let tagEnd: number = 0;
      let contentStart: number | undefined = undefined;
      for (let i = 0; i < sym.length; i++) {
        if (sym[i] === "$") {
          if (tagStart === undefined) {
            parts.push({ type: "string", value: sym.slice(tagEnd, i) });
            tagStart = i;
          }
          continue;
        }
        if (tagStart !== undefined) {
          if (sym[i] === "[") {
            contentStart = i + 1;
          } else if (sym[i] === "]") {
            if (contentStart === undefined) throw new Error("Bad quoting");
            tagEnd = i + 1;
            parts.push({ type: "tag", value: sym.slice(contentStart, i) });
            tagStart = undefined;
          }
        }
      }

      return { type: "quotevarinterpolated", value: parts, original };
    }
    function parseApply(): AstExpr | undefined {
      const args: Array<AstExpr> = [];
      while (true) {
        eatWhitspace();
        const matches = matchEat(")");
        if (matches === undefined)
          throw new Error("parseApply: Unexpected eof");
        if (matches) break;
        const expr = parseAstExpr();
        if (expr === undefined) break;
        args.push(expr);
      }
      return makeApply(args);
    }
    function parseSym(char: string): string | undefined {
      let sym = char;
      idx += 1;
      while (true) {
        const p = peek();
        if (p === undefined) throw new Error("parseSym: Unexpected EOF");
        if (isWhitespace(p) || p === ")" || p === "(") {
          return sym;
        }
        sym += p;
        idx++;
      }
    }
    function parseString(char: string): string | undefined {
      if (char !== '"') return undefined;
      const quotType = char;
      idx += 1;
      let theString = "";
      while (idx <= src.length) {
        const matches = matchEat(quotType);
        if (!matchEat) throw new Error("parseString: Unexpected eof");
        if (matches) break;
        theString += src[idx++];
      }
      return theString;
    }
    function parseNumber(char: string): number | undefined {
      const start = 48;
      const startNoZero = 49;
      const end = 57;
      let charCode = char.charCodeAt(0);
      // is zero
      if (charCode === start) {
        if (
          src[idx + 1] === undefined ||
          !(charCode >= startNoZero && charCode <= end)
        ) {
          idx++;
          return 0;
        }
      }
      if (charCode >= startNoZero && charCode <= end) {
        let numValue = 0;
        while (true) {
          const p = peek();
          if (p === undefined) throw new Error("parseNumber: Unexpected EOF");
          charCode = p.charCodeAt(0);
          if (charCode >= start && charCode <= end) {
            numValue *= 10;
            numValue += parseInt(p);
            idx += 1;
          } else {
            return numValue;
          }
        }
      }
      return undefined;
    }
    function eatWhitspace() {
      while (true) {
        const p = peek();
        if (p === undefined) return;
        if (!isWhitespace(p)) return;
        idx++;
      }
    }
    function isWhitespace(char: string): boolean {
      return char === " " || char === "\n" || char === "\r" || char === "\t";
    }
    // Main parse loop
    while (idx < src.length) {
      const AstExpr = parseAstExpr();
      if (!AstExpr) break;
      AstExprs.push(AstExpr);
    }
    return AstExprs;
  };
  const run = (exprs: Array<AstExpr>) => {
    const makeValStr = (str: string): LispValue => ({
      value: str,
      type: "str",
    });
    const makeValNum = (num: number): LispValue => ({
      value: num,
      type: "num",
    });
    const makeValSym = (sym: string): LispValue => ({
      value: sym,
      type: "sym",
    });
    const makeValSymp = (sym: string): LispValue => ({
      value: sym,
      type: "symp",
    });
    const makeValNull = (): LispValue => ({ type: "null", value: null });
    const makeValLambda = (
      args: string[],
      containsSpreadArg: boolean,
      code: AstExpr
    ): LispValue => ({
      value: { args, code, containsSpreadArg },
      type: "lambda",
    });
    const makeValBuiltin = (name: string): LispValue => ({
      value: name,
      type: "builtin",
    });
    const makeValPlist = (values: Record<string, LispValue>): LispValue => ({
      value: values,
      type: "plist",
    });
    const makeValArray = (values: Array<LispValue>): LispValue => ({
      value: values,
      type: "list",
    });
    type Env = Record<string, LispValue>;
    const initialEnv = (): [
      env: Env,
      argMap: Record<string, { args: number; hasRest: boolean }>,
      macros: Record<string, { args: string[]; body: Array<AstExpr> }>
    ] => {
      const env: Env = {};
      const builtinArgMap: Record<string, { args: number; hasRest: boolean }> =
        {};
      const builtins: Array<[op: string, args: number, hasRest?: boolean]> = [
        ["-", 2, false],
        ["+", 2, true],
        ["++", 2, true],
        ["*", 2],
        ["/", 2],
        ["symbol", 1, true],
        ["car", 1],
        ["cdr", 1],
        ["string", 1],
        ["print", 0, true],
        ["setq", 2],
        ["defun", 2],
        ["defmacro", 3],
        ["let", 2],
        ["if", 3],
        ["<=", 2],
        ["=", 2],
        ["assert-eq", 2],
        ["plist", 1, true],
        ["set", 2, true],
        ["get", 2],
        ["list", 1, true],
        ["list-get", 2],
        ["length", 1],
        ["append", 1, true],
        ["apply", 2],
      ];
      builtins.forEach(([op, args, hasRest]) => {
        env[op] = makeValBuiltin(op);
        Object.assign(builtinArgMap, {
          [op]: { args, hasRest: hasRest === undefined ? false : hasRest },
        });
      });
      env["true"] = makeValNum(1);
      env["false"] = makeValNum(0);
      env["null"] = makeValSym("null");
      return [env, builtinArgMap, {}];
    };
    const valToString = (val: LispValue): string => {
      switch (val.type) {
        case "list": {
          return JSON.stringify(val.value.map((value) => valToString(value)));
        }
        case "plist": {
          return JSON.stringify(
            Object.keys(val.value).reduce(
              (acc, cur) => ({ ...acc, [cur]: valToString(val.value[cur]) }),
              {}
            )
          );
        }
        case "str":
        case "num":
        case "sym":
          return String(val.value);
        case "symp":
          return "'" + val.value;
        case "lambda":
          return `lambda unknown`;
        case "builtin":
          return "builtin";
        case "null":
          return "null";
      }
    };
    const cloneEnv = (env: Env) => ({ ...env });
    const envGet = (env: Env, key: string): LispValue => {
      const value = env[key];
      if (!value) throw new Error(`Undefined variable ${key}`);
      return value;
    };
    const envGetSymp = (env: Env, symp: string): LispValue => envGet(env, symp);
    const envSetFromExpr = (env: Env, key: LispValue, value: LispValue) => {
      if (key.type !== "str")
        throw new Error(`Expected lisp string got: ${key.type}`);
      env[key.value] = value;
    };
    const isEqual = (a: LispValue, b: LispValue): boolean => {
      if (a.type !== b.type) return false;
      switch (a.type) {
        case "list": {
          if (b.type !== "list") return false;
          if (b.value.length !== a.value.length) return false;
          for (let i = 0; i < a.value.length; i++) {
            if (!isEqual(a.value[i], b.value[i])) return false;
          }
          return true;
        }
        case "plist": {
          if (b.type != "plist") return false;
          if (Object.keys(a.value).length !== Object.keys(b.value).length)
            return false;
          for (const key of Object.keys(a.value)) {
            const exists = key in b.value;
            if (!exists) return false;
            const aval = a.value[key];
            const bval = b.value[key];
            if (!isEqual(aval, bval)) return false;
          }
          return true;
        }
        case "num":
        case "str":
        case "sym":
        case "symp":
          return a.value === b.value;
        case "lambda":
        case "builtin":
          return false;
        case "null":
          return true;
      }
    };
    const isTruthy = (env: Env, val: LispValue): boolean => {
      switch (val.type) {
        case "num": {
          return val.value !== 0;
        }
        case "list":
        case "plist":
        case "str":
        case "builtin":
          return true;
        case "sym":
          const symbol = envGet(env, val.value);
          return isTruthy(env, symbol);
        case "lambda":
          // All lambdas are truthy
          return true;
        default:
          return false;
      }
    };
    const isValidPlist = (potentialPlist: LispValue): Plist => {
      if (potentialPlist.type === "null") return {};
      if (potentialPlist.type === "plist") return potentialPlist.value;
      throw new Error("Not a plist");
    };
    function evaluate(env: Env, expr: AstExpr): LispValue {
      switch (expr.type) {
        case "macroapp": {
          return evaluateMacroApp(env, expr);
        }
        case "quotevarinterpolated":
        case "quotevar": {
          throw new Error("BAD eval of a quote var");
        }
        case "str": {
          return makeValStr(expr.value[0]);
        }
        case "app": {
          return evaluateApp(env, expr);
        }
        case "num": {
          return makeValNum(expr.value[0]);
        }
        case "symp": {
          return makeValSymp(expr.value[0]);
        }
        case "sym": {
          if (expr.value[0] === "null") return makeValNull();
          return envGet(env, expr.value[0]);
        }
      }
    }

    function lispValToExpr(val: LispValue): AstExpr {
      switch (val.type) {
        case "str":
          return { type: "str", value: [val.value] };
        case "num":
          return { type: "num", value: [val.value] };
        case "sym":
          return { type: "sym", value: [val.value] };
        case "symp":
          return { type: "symp", value: [val.value] };
        case "list":
          return { type: "app", value: val.value.map(lispValToExpr) };
        default:
          throw new Error(`Unsupported LispValue type: ${JSON.stringify(val)}`);
      }
    }

    function evaluateMacroApp(env: Env, macroApp: MacroApp): LispValue {
      const [macroName, ...argsExpr] = macroApp.value;
      if (macroName.type !== "sym") throw new Error("Invalid macro name");
      const macro = macros[macroName.value[0]];
      //   const args = argsExpr.map((expr) => evaluate(env, expr));
      if (macro.args.length !== argsExpr.length)
        throw new Error("Invalid macro args length");
      let macroArgs = {};
      for (let i = 0; i < macro.args.length; i++) {
        macroArgs[macro.args[i]] = argsExpr[i];
      }
      let result: LispValue = makeValNull();
      for (const body of macro.body) {
        const codeToRun = substituteMacroBody(macroArgs, body);
        result = evaluate(env, codeToRun);
      }
      return result;
    }

    function substituteMacroBody(
      macroArgs: Record<string, AstExpr>,
      body: AstExpr
    ): AstExpr {
      switch (body.type) {
        case "str": {
          return body;
        }
        case "num":
          return body;
        case "sym":
          return body;
        case "symp":
          if (body.value[0][0] === "#") {
            const name = body.value[0].slice(1);
            const expr = macroArgs[name];
            if (expr.type !== "sym") throw new Error("bad quote in symp");
            return {
              type: "symp",
              value: [expr.value[0]],
            };
          }
          return body;
        case "app":
          return {
            type: "app",
            value: body.value.map((expr) =>
              substituteMacroBody(macroArgs, expr)
            ),
          };
        case "macroapp":
          throw new Error("Macro inside macro not supported");
        case "quotevarinterpolated": {
          let fullString: string = "";
          for (const part of body.value) {
            if (part.type === "string") {
              fullString += part.value;
              continue;
            }
            const name = part.value;
            const newExpr = macroArgs[name];
            if (newExpr === undefined)
              throw new Error(`Quote var not found ${name}`);
            switch (newExpr.type) {
              case "sym":
              case "str":
              case "symp": {
                fullString += newExpr.value[0];
                break;
              }
              default: {
                throw new Error(
                  `Quote part cant be substituted with ${newExpr.type}`
                );
              }
            }
          }
          if (body.original === "sym") {
            return makeSym(fullString);
          } else if (body.original === "symp") {
            return makeSymp(fullString);
          }
          throw new Error("unreachable");
        }
        case "quotevar": {
          const name = body.value[0];
          const newExpr = macroArgs[name];
          if (newExpr === undefined)
            throw new Error(`Quote var not found ${name}`);
          return newExpr;
        }
      }
    }

    function evaluateBuiltin(
      env: Env,
      builtin: string,
      args: Array<AstExpr>
    ): LispValue {
      switch (builtin) {
        case "symbol": {
          const argVals = args.map((a) => evaluate(env, a));
          const newsymbol = argVals.reduce((a, curr) => {
            if (curr.type !== "str")
              throw new Error(`Not a string ${curr.value}`);
            return a + curr;
          }, "");
          return makeValSymp(newsymbol);
        }
        case "string": {
          const [valueExpr] = args;
          const value = evaluate(env, valueExpr);
          switch (value.type) {
            case "num":
              return makeValStr(value.value + "");
            case "null":
              return makeValStr("null");
            case "str":
              return value;
            case "sym":
            case "symp":
              return makeValStr(value.value);
            default:
              throw new Error(`Can't turn ${value.type} into a string`);
          }
        }
        case "defmacro": {
          const [macroName, macroArgs, ...macroBody] = args;
          if (macroName.type !== "sym") throw new Error("bad macro name");
          if (macroArgs.type !== "app") throw new Error("bad macro args");

          const argNames = macroArgs.value.map((expr) => {
            if (expr.type !== "sym") throw new Error("macro args bad");
            return expr.value[0];
          });
          macros[macroName.value[0]] = {
            args: argNames,
            body: macroBody,
          };
          return makeValNull();
        }
        case "append": {
          const [listExpr, ...restExpr] = args;
          const list = evaluate(env, listExpr);
          if (!(list.type === "list" || list.type === "null"))
            throw new Error("First arg of `append` should be a list or null");
          const actualList = list.type === "null" ? [] : list.value;
          const appendValues = restExpr.map((expr) => evaluate(env, expr));
          return makeValArray([...actualList, ...appendValues]);
        }
        case "apply": {
          const [fnExpr, ...argsExpr] = args;
          const fnSymp = evaluate(env, fnExpr);
          if (fnSymp.type !== "symp")
            throw new Error("Apply first arg should be symp");
          if (argsExpr.length > 1)
            throw new Error("Apply requires 2 arguments");
          const argsVal = evaluate(env, argsExpr[0]);
          if (argsVal.type !== "list")
            throw new Error("Apply second arg should be a list");
          const fn = envGetSymp(env, fnSymp.value);
          if (fn.type === "builtin") {
            return evaluateBuiltin(
              env,
              fn.value,
              argsVal.value.map((val) => lispValToExpr(val))
            );
          }
          if (fn.type !== "lambda")
            throw new Error("First arg to `apply` must be a lambda");
          const lambdaEnv = addArgsToEnv(env, fn, argsExpr);
          return evaluate(lambdaEnv, fn.value.code);
        }
        case "length": {
          const [valExpr] = args;
          const val = evaluate(env, valExpr);
          switch (val.type) {
            case "str":
              return makeValNum(val.value.length);
            case "plist":
              return makeValNum(Object.keys(val.value).length);
            case "list":
              return makeValNum(val.value.length);
            default: {
              throw new Error("Length arg is not str, plist, or list");
            }
          }
        }
        case "plist": {
          const keyVals = args;
          let plist: Plist = {};
          for (let i = 0; i < keyVals.length; i += 2) {
            const keyExpr = keyVals[i];
            const valExpr = keyVals[i + 1];
            let keyLispVal = evaluate(env, keyExpr);
            if (keyLispVal.type === "symp") {
              keyLispVal = makeValStr(keyLispVal.value);
            }
            if (keyLispVal.type !== "str") {
              throw new Error(
                `Plist key must be a symp or str, got: ${valToString(
                  keyLispVal
                )}`
              );
            }
            const val = evaluate(env, valExpr);
            plist[keyLispVal.value] = val;
          }
          return makeValPlist(plist);
        }
        case "get": {
          const [maybePlist, keyExpr] = args;
          const plist = isValidPlist(evaluate(env, maybePlist));
          const key = evaluate(env, keyExpr);
          if (key.type !== "str" || key.value !== "symp")
            throw new Error("Key in `get` should be a symp or str");
          return plist[key.value] || makeValNull();
        }
        case "set": {
          const [potentialPlist, ...keyVals] = args;
          let plist = { ...isValidPlist(evaluate(env, potentialPlist)) };
          if (keyVals.length === 0 || keyVals.length % 2 !== 0)
            throw new Error("Set requires valid key val pair");
          for (let i = 0; i < keyVals.length; i += 2) {
            const keyExpr = keyVals[i];
            const valExpr = keyVals[i + 1];
            let keyLispVal = evaluate(env, keyExpr);
            if (keyLispVal.type === "symp") {
              keyLispVal = makeValStr(keyLispVal.value);
            }
            if (keyLispVal.type !== "str") {
              throw new Error(
                `Plist key must be a symp or str, got: ${valToString(
                  keyLispVal
                )}`
              );
            }
            const val = evaluate(env, valExpr);
            plist[keyLispVal.value] = val;
          }
          return makeValPlist(plist);
        }
        case "cdr": {
          const [listExpr] = args;
          const list = evaluate(env, listExpr);
          if (list.type !== "list")
            throw new Error("First arg in `cdr` is not a list");
          return makeValArray(list.value.slice(1));
        }
        case "car": {
          const [listExpr] = args;
          const list = evaluate(env, listExpr);
          if (list.type !== "list")
            throw new Error("First arg in `car` is not a list");
          return list.value[0] || makeValNull();
        }
        case "list-get": {
          const [listExpr, idxExpr] = args;
          const list = evaluate(env, listExpr);
          if (list.type !== "list")
            throw new Error("First arg in `list-get` is not a list");
          const idx = evaluate(env, idxExpr);
          if (idx.type !== "num")
            throw new Error("Second arg in `list-get` is not a number");
          return list.value[Math.floor(idx.value)] || makeValNull();
        }
        case "list": {
          // (list a b c d)
          if (args.length === 0)
            throw new Error("`list` expects at least 1 arg");

          const values: Array<LispValue> = [];
          for (const arg of args) {
            values.push(evaluate(env, arg));
          }
          return makeValArray(values);
        }
        case "defun": {
          // (defun foo (a b) (+ a b))
          const [fnNameExpr, fnArgs, body] = args;
          if (fnNameExpr === undefined) throw new Error("No fn name");
          if (fnNameExpr.type !== "sym")
            throw new Error(
              `Fn name should be a symbol not a ${fnNameExpr.type}`
            );
          if (fnArgs === undefined) throw new Error("No fn args");
          if (body === undefined) throw new Error("no function body");
          if (fnArgs.type !== "app") throw new Error("Fn args expected list");
          const last = fnArgs.value.length === 0 ? 0 : fnArgs.value.length - 1;
          let containsSpreadArg = false;
          const fnName = makeValStr(fnNameExpr.value[0]);
          envSetFromExpr(
            env,
            fnName,
            makeValLambda(
              [
                ...fnArgs.value.map((f: AstExpr, i) => {
                  if (f.type !== "sym") throw new Error("arg must be sym");
                  const isSpread = f.value[0].indexOf("...") === 0;
                  if (isSpread && i !== last)
                    throw new Error("Spread arg must be last arg");
                  if (isSpread && f.value[0].length === 3)
                    throw new Error("`...` is invalid for a spread arg");
                  if (isSpread) {
                    containsSpreadArg = true;
                    return f.value[0].slice(3);
                  }
                  return f.value[0];
                }),
              ],
              containsSpreadArg,
              body
            )
          );
          return makeValNull();
        }
        case "++": {
          const sum = args
            .map((arg) => evaluate(cloneEnv(env), arg))
            .reduce((acc, cur) => {
              if (cur.type !== "str") throw new Error("Expected string");
              return acc + cur.value;
            }, "");
          return makeValStr(sum);
        }
        case "+": {
          const sum = args
            .map((arg) => evaluate(cloneEnv(env), arg))
            .reduce((acc, cur) => {
              if (cur.type !== "num") throw new Error("Expected num");
              return acc + cur.value;
            }, 0);
          return makeValNum(sum);
        }
        case "let": {
          const [bindings, body] = args;
          if (
            !bindings ||
            bindings.type !== "app" ||
            bindings.value.length === 0
          )
            throw new Error("No bindings!");
          if (!body) throw new Error("No body!");
          const newEnv = cloneEnv(env);
          bindings.value.forEach((binding) => {
            if (binding.type !== "app") throw new Error("Invalid binding");
            const [name, valueExpr] = binding.value;
            if (name === undefined || valueExpr === undefined)
              throw new Error("Binding expected a name and a value expr");
            if (name.type !== "sym")
              throw new Error("Binding name just be a symbol");
            const value = evaluate(cloneEnv(newEnv), valueExpr);
            envSetFromExpr(newEnv, makeValStr(name.value[0]), value);
          });
          return evaluate(newEnv, body);
        }
        case "if": {
          const [cond, then, ...elze] = args;
          if (cond === undefined || then === undefined || elze === undefined)
            throw new Error("Bad if");
          const condVal = evaluate(env, cond);
          if (isTruthy(env, condVal)) {
            return evaluate(env, then);
          } else {
            const tempEnv = env;
            let result = makeValNull();
            for (const expr of elze) {
              result = evaluate(tempEnv, expr);
            }
            return result;
          }
        }
        case "print": {
          const valueStrings = args
            .map((arg) => evaluate(env, arg))
            .map(valToString);
          console.log(valueStrings.join(" "));
          return makeValNull();
        }
        case "=": {
          const [a, b] = args;
          if (a === undefined || b === undefined) throw new Error('Bad "="');
          const [aval, bval] = args.map((arg) => evaluate(env, arg));
          if (aval === undefined || bval === undefined)
            throw new Error('Bad "="');
          return isEqual(aval, bval) ? makeValNum(1) : makeValNum(0);
        }
        case "<=": {
          const [a, b] = args;
          if (a === undefined || b === undefined) throw new Error('Bad "<="');
          const [aval, bval] = args.map((arg) => evaluate(env, arg));
          if (aval?.type !== "num" || bval?.type !== "num")
            throw new Error('Bad "<="');
          return aval.value <= bval.value ? makeValNum(1) : makeValNum(0);
        }
        case "assert-eq": {
          const [a, b] = args;
          if (a === undefined || b === undefined) throw new Error('Bad "="');
          const [aval, bval] = args.map((arg) => evaluate(env, arg));
          if (aval === undefined || bval === undefined)
            throw new Error('Bad "="');
          if (isEqual(aval, bval)) return makeValNum(1);
          throw new Error(
            `Assertion failed: ${valToString(aval)} != ${valToString(bval)}`
          );
        }
        default: {
          throw new Error(`Unhandled builtin: ${builtin}`);
        }
      }
    }

    function addArgsToEnv(
      env: Env,
      fn: { type: "lambda"; value: Lambda },
      args: AstExpr[]
    ): Env {
      const containsSpread: boolean = fn.value.containsSpreadArg;
      const last: number =
        fn.value.args.length === 0 ? 0 : fn.value.args.length - 1;
      const argEnv = cloneEnv(env);
      const spreadArgs = containsSpread
        ? args.slice(last).map((arg) => evaluate(env, arg))
        : undefined;
      let lambdaEnv = cloneEnv(env);
      fn.value.args.forEach((arg, idx) => {
        if (containsSpread && idx >= last) {
          lambdaEnv[arg] = makeValArray(spreadArgs!);
        } else {
          const val = evaluate(argEnv, args[idx]!);
          lambdaEnv[arg] = val;
        }
      });
      return lambdaEnv;
    }

    function evaluateApp(env: Env, app: App): LispValue {
      const [fnExpr, ...args] = app.value;
      if (fnExpr === undefined) throw new Error("No fn expr");
      const fn = evaluate(env, fnExpr);
      if (fn.type === "builtin") return evaluateBuiltin(env, fn.value, args);
      if (fn.type === "lambda") {
        const lambdaEnv = addArgsToEnv(env, fn, args);
        return evaluate(lambdaEnv, fn.value.code);
      }
      throw new Error(`Invalid function value: ${fn.type}`);
    }

    const [__ENV, argMap, macros] = initialEnv();
    let ENV = __ENV;
    let result: LispValue | undefined = undefined;
    for (const expr of exprs) {
      const tempEnv = cloneEnv(ENV);
      result = evaluate(tempEnv, expr);
      ENV = tempEnv;
    }
    return result;
  };
  const values = parse(src);
  if (onlyParse) return values;
  return run(values);
};

const val = lisp(
  `
  (defmacro html-tag (tag-name)
    (defun #tag-name (...props)
        (append (list '#tag-name) (apply 'plist props))))
  
  (defun render-el (el) 
    (++ 
        "<" (string (car el)) ">"
        (let ((body) (get el :body))
          (if body (render-el body)) else "")
        "</" (string (car el)) ">" ))

  (html-tag! h1)
  (html-tag! h2)
  (html-tag! h3)
  (html-tag! h4)
  (html-tag! h5)
  (html-tag! h6)
  (html-tag! p)
  (print (render-el (h1 :class "text-sm" :body (p :class "NICE"))))
  (print (h1 :class "text-sm" :body "HELLO"))
`,
  false
);
console.log("VAL", val);
