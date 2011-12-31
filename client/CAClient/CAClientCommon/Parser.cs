using System;
using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;
using System.Text;
using System.Text.RegularExpressions;
using System.IO;
using ParserCombinators;

namespace CAClientCommon
{
    // ADT.
    public abstract class Value
    {
        public static string bracket(string before, string after, Action<StringBuilder> between)
        {
            var sb = new StringBuilder();
            sb.Append(before);

            between(sb);

            sb.Append(after);
            return sb.ToString();
        }
    };
    // Objects
    public class ASObject : Value
    {
        public readonly ASPair[] values;
        public readonly string className;
        public ASObject(string c, ASPair[] vals) { className = c; values = vals; }
        public override string ToString()
        {
            return bracket("{", "}", (sb) =>
            {
                bool first = true;
                foreach (ASPair p in values)
                {
                    if (first)
                        first = false;
                    else
                        sb.Append(',');
                    sb.Append(p);
                }
            });
        }

        public Value lookup(string k)
        {
            foreach (ASPair p in values)
            {
                if (p.Key == k)
                    return p.Value;
            }

            return null;
        }
    }

    // Arrays.
    public class ASArray : Value
    {
        public readonly Value[] values;
        public ASArray(Value[] vals) { values = vals; }
        public override string ToString()
        {
            return bracket("[", "]", (sb) =>
            {
                bool first = true;
                foreach (Value p in values)
                {
                    if (first)
                        first = false;
                    else
                        sb.Append(',');
                    sb.Append(p);
                }
            });
        }
    }

    // Simple data types.
    public class ASString : Value
    {
        public readonly string String;
        public ASString(string val) { String = val; }
        public override string ToString()
        {
            return bracket("\"", "\"", (sb) =>
            {
                sb.Append(String);
            });
        }

    }
    public class ASNull : Value { public override string ToString() { return "null"; } };
    public class ASNaN : Value { public override string ToString() { return "NaN"; } };
    public class ASBool : Value
    {
        public readonly bool value;
        public ASBool(bool val) { value = val; }
        public override string ToString() { return value.ToString(); }
    };
    public class ASNumber : Value
    {
        public readonly double value;
        public ASNumber(double d) { value = d; }
        public ASNumber(int i) { value = Convert.ToDouble(i); }
        public override string ToString()
        {
            if (value == Math.Truncate(value))
                return Convert.ToInt32(value).ToString();
            else
                return value.ToString();
        }
    }
    public class ASOther : Value
    {
        public readonly string value;
        public ASOther(string s) { value = s; }
        public override string ToString()
        {
            return bracket("\"", "\"", (sb) =>
            {
                sb.Append(value);
            });
        }
    }

    // SUpporting types
    public class ASPair
    {
        public readonly string Key;
        public readonly Value Value;
        public ASPair(string key, Value val) { Key = key; Value = val; }
        public override string ToString()
        {
            var sb = new StringBuilder();

            sb.Append(Key);
            sb.Append(':');
            sb.Append(Value);

            return sb.ToString();
        }
    }

    public abstract class ASParsers<TInput> : CharParsers<TInput>
    {
        public Parser<TInput, string> Literal;
        public Parser<TInput, string> Ident;
        public Parser<TInput, string> ObjectId;

        public Parser<TInput, Value> Number;
        public Parser<TInput, Value> StrLit;
        public Parser<TInput, Value> Null;
        public Parser<TInput, Value> NaN;
        public Parser<TInput, ASBool> True;
        public Parser<TInput, ASBool> False;
        public Parser<TInput, Value> Boolean;
        public Parser<TInput, Value> Other;

        public Parser<TInput, ASPair> Pair;
        public Parser<TInput, Value> Obj;

        public Parser<TInput, Value> ArrayPair;
        public Parser<TInput, Value> Array;

        public Parser<TInput, Value> Val;
        public ASParsers()
        {
            // Identifier.
            Ident = from first in Char(char.IsLetter).OR(Char('_'))
                    from rest in Rep(Char(char.IsLetterOrDigit).OR(Char('_')).OR(Char('.')))
                    select rest.Aggregate(first.ToString(), (acc, ch) => acc + ch);

            ObjectId = from p1 in Char('(')
                       from package in Ident
                       from sep in Str("::")
                       from objClass in Ident
                       from p2 in Char(')')
                       select package + sep + objClass;

            // Numbers
            Number = (from d in Decimal()
                      select (Value)new ASNumber(d)
                     ).OR(
                      from i in Integer()
                      select (Value)new ASNumber(i)
                      );

            // String literals.
            StrLit = from q1 in Char('"')
                     from cs in Rep(Char((c) => !(c == '"')))
                     from q2 in Char('"')
                     select (Value)new ASString(new String(cs.ToArray()));

            // Boolean
            True = from c1 in Str("true") select new ASBool(true);
            False = from str in Str("false") select new ASBool(false);
            Boolean = from b in True.OR(False) select (Value)b;

            // Nulls
            Null = from str in Str("(null)")
                   select (Value)new ASNull();
            NaN = from str in Str("NaN")
                  select (Value)new ASNaN();

            // Other
            Other = from cs in Rep1(AnyChar)
                    select (Value)new ASOther(new String(cs.ToArray()));

            // Object.
            Pair = from indent in Indent()
                   from id in Ident
                   from w2 in Whitespace()
                   from eq in Char('=')
                   from w3 in Whitespace()
                   from v in Val
                   select new ASPair(id, v);

            Obj = from id in ObjectId
                  from c1 in Char('#')
                  from idnum in Integer()
                  from pairs in WithIndent(2, Rep(Pair))
                  select (Value)new ASObject(id, pairs.ToArray());

            // Array.
            ArrayPair = from indent in Indent()
                        from b1 in Char('[')
                        from i in Integer()
                        from b2 in Char(']')
                        from w in Whitespace()
                        from v in Val
                        select v;
            Array = from str in Str("(Array)#")
                    from id in Integer()
                    from vals in WithIndent(2, Rep(ArrayPair))
                    select (Value)new ASArray(vals.ToArray());


            // Value types.
            Val = StrLit.OR(Boolean).OR(Null).OR(NaN).OR(Number).OR(Array).OR(Obj).OR(Other);

        }
    }

    public abstract class LogParsers<TInput> : ASParsers<TInput>
    {
        public abstract class LogMsg
        {
            public DateTime time;
            public string level;
            public Value data;

        };

        private class SendingMessage : LogMsg
        {
            public SendingMessage(DateTime t, string l, Value d) { time = t; level = l; data = d; }
            public override string ToString()
            {
                return "SendingMessage: " + time.ToString() + ":" + data.ToString();
            }
        }

        private class EndOfGameStatsMsg : LogMsg
        {
            public EndOfGameStatsMsg(DateTime t, string l, Value d) { time = t; level = l; data = d; }
            public override string ToString()
            {
                return "EndOfGameStats: " + time.ToString() + ":" + data.ToString();
            }
        }
 
        private class OtherLogMsg : LogMsg
        {
            public OtherLogMsg() { }
            public OtherLogMsg(DateTime t, string l) { time = t; level = l; }
            public override string ToString()
            {
                return "OtherLogMsg: " + time.ToString();
            }
        }

        public LogMsg classifyMessage(DateTime t, string l, Value v)
        {
            try
            {
                ASObject body = (ASObject)((ASObject)v).lookup("body");
                if (body.className == "com.riotgames.platform.gameclient.domain::EndOfGameStats")
                {
                    return new EndOfGameStatsMsg(t, l, body);
                }
                else
                {
                    return new SendingMessage(t, l, v);
                }
            }
            catch (Exception e)
            {
                return new SendingMessage(t, l, v);
            }
        }

        public Parser<TInput, DateTime> DateTime;
        public Parser<TInput, string> Level;
        public Parser<TInput, LogMsg> AsyncMessage;
        public Parser<TInput, LogMsg> SentMessage;
        public Parser<TInput, LogMsg> OtherMessage;
        public Parser<TInput, LogMsg> LogLine;
        public Parser<TInput, DList<LogMsg>> LogMsgs;

        public LogParsers()
        {
            DateTime = from m in Integer()
                       from c1 in Char('/')
                       from d in Integer()
                       from c2 in Char('/')
                       from y in Integer()
                       from w in Whitespace()
                       from H in Integer()
                       from c3 in Char(':')
                       from M in Integer()
                       from c4 in Char(':')
                       from S in Integer()
                       from c5 in Char('.')
                       from U in Integer()
                       select new DateTime(y, m, d, H, M, S, U);

            Level = from c1 in Char('[')
                    from l in Str("INFO").OR(Str("DEBUG")).OR(Str("WARN")).OR(Str("ERROR")).OR(Str("FATAL"))
                    from c2 in Char(']')
                    select l;

            AsyncMessage = from t in DateTime
                           from w1 in Whitespace()
                           from l in Level
                           from w2 in Whitespace()
                           from prefix in Str("com.riotgames.platform.gameclient.module.services.RemoteObjectGenerator Got async message:")
                           from w3 in Whitespace()
                           from val in Val
                           select classifyMessage(t,l,val);
            SentMessage = from t in DateTime
                          from w1 in Whitespace()
                          from l in Level
                          from w2 in Whitespace()
                          from prefix in Str("com.riotgames.platform.gameclient.module.services.RemoteObjectGenerator Sending message:")
                          from w3 in Whitespace()
                          from val in Val
                          select (LogMsg) new OtherLogMsg(t, l);
            OtherMessage = from t in DateTime
                           from w1 in Whitespace()
                           from l in Level
                          from w2 in Whitespace()from c2 in Rep(Char((c) => (c != '\n')))
                           from nl in Char('\n')
                           select (LogMsg)new OtherLogMsg(t, l);

            LogLine = from msg in AsyncMessage.OR(SentMessage).OR(OtherMessage)
                      select msg;
            LogMsgs = Rep(LogLine);
        }
    }

}
