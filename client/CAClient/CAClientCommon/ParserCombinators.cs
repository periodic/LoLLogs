using System;
using System.Linq;
using System.Collections.Generic;
using System.Text;

namespace ParserCombinators
{
    public class DList<T> : LinkedList<T> {
        public DList() { }
        public DList(IEnumerable<T> data) {
            foreach (T item in data) {
                AddLast(item);
            }
        }
        public DList<T> push(T item)
        {
            AddLast(item);
            return this;
        }
        public T pop()
        {
            T val = Last.Value;
            RemoveLast();
            return val;
        }
        public T shift()
        {
            T val = First.Value;
            RemoveFirst();
            return val;
        }
        public DList<T> unshift(T item)
        {
            AddFirst(item);
            return this;
        }
    }

    // The result of a parse consists of a value and the unconsumed input.
    public class Result<TInput, TValue>
    {
        public readonly TInput Rest;
        public readonly TValue Value;
        public Result(TValue value, TInput rest) { Value = value; Rest = rest; }
    }

    // A Parser is a delegate which takes an input and returns a result.
    public delegate Result<TInput, TValue> Parser<TInput, TValue>(TInput input, int indent);

    public static class ParserCombinatorExtensions
    {
        public static Parser<TInput, TValue> OR<TInput, TValue>(
            this Parser<TInput, TValue> parser1,
            Parser<TInput, TValue> parser2)
        {
            return (input, indent) => parser1(input, indent) ?? parser2(input, indent);
        }
        public static Parser<TInput, TValue2> AND<TInput, TValue1, TValue2>(
            this Parser<TInput, TValue1> parser1,
            Parser<TInput, TValue2> parser2)
        {
            return (input,indent) => {
                var res = parser1(input, indent);
                if (res == null)
                    return null;
                else 
                    return parser2(res.Rest, indent);
            };
        }
    }

    public static class ParserCombinatorsMonad
    {
        // By providing Select, Where and SelectMany methods on Parser<TInput,TValue> we make the 
        // C# Query Expression syntax available for manipulating Parsers.  
        public static Parser<TInput, TValue> Where<TInput, TValue>(
            this Parser<TInput, TValue> parser,
            Func<TValue, bool> pred)
        {
            return (input, indent) =>
            {
                var res = parser(input, indent);
                if (res == null || !pred(res.Value)) return null;
                return res;
            };
        }
        public static Parser<TInput, TValue2> Select<TInput, TValue, TValue2>(
            this Parser<TInput, TValue> parser,
            Func<TValue, TValue2> selector)
        {
            return (input, indent) =>
            {
                var res = parser(input, indent);
                if (res == null) return null;
                return new Result<TInput, TValue2>(selector(res.Value), res.Rest);
            };
        }
        public static Parser<TInput, TValue2> SelectMany<TInput, TValue, TIntermediate, TValue2>(
            this Parser<TInput, TValue> parser,
            Func<TValue, Parser<TInput, TIntermediate>> selector,
            Func<TValue, TIntermediate, TValue2> projector)
        {
            return (input, indent) =>
            {
                var res = parser(input, indent);
                if (res == null) return null;
                var val = res.Value;
                var res2 = selector(val)(res.Rest, indent);
                if (res2 == null) return null;
                return new Result<TInput, TValue2>(projector(val, res2.Value), res2.Rest);
            };
        }
    }

    // Contains all the basic parsers that are independent of return type.
    public abstract class Parsers<TInput>
    {
        public Parser<TInput, TValue> Succeed<TValue>(TValue value)
        {
            return (input,indent) => new Result<TInput, TValue>(value, input);
        }
        public Parser<TInput, DList<TValue>> Rep<TValue>(Parser<TInput, TValue> parser)
        {
            return Rep1(parser).OR(Succeed(new DList<TValue>()));
        }
        public Parser<TInput, DList<TValue>> Rep1<TValue>(Parser<TInput, TValue> parser)
        {
            return from x in parser
                   from xs in Rep(parser)
                   select xs.unshift(x);
        }
    }

    // Contains a few parsers that parse characters from an input stream
    public abstract class CharParsers<TInput> : Parsers<TInput>
    {
        public abstract Parser<TInput, char> AnyChar { get; }
        public Parser<TInput, char> Char(char ch)
        {
            return from c in AnyChar where c == ch select c;
        }
        public Parser<TInput, char> Char(Predicate<char> pred)
        {
            return from c in AnyChar where pred(c) select c;
        }

        public Parser<TInput, string> Str(string str)
        {
            return from cs in Str(new DList<char>(str.ToArray()))
                   select new String(cs.ToArray());
        }
        public Parser<TInput, DList<char>> Str(DList<char> str)
        {
            if (str.Count() == 0) {
                return Succeed(new DList<Char>());
            } else {
                var c = str.shift();

                return from s in Char(c)
                       from ss in Str(str)
                       select ss.unshift(s);
           }
        }
        public Parser<TInput, string> Indent()
        {
            return Char('\n').AND((input, indent) => Str(new String(' ', indent))(input, indent));
        }
        public Parser<TInput, TValue> WithIndent<TValue>(int step, Parser<TInput,TValue> parser) 
        {
            return (input, indent) => parser(input, indent + step);
        }

        public Parser<TInput, DList<char>> Whitespace()
        {
            return Rep(Char(' ').OR(Char('\t')));
        }
        public Parser<TInput, int> Integer()
        {
            return from digits in Rep1(Char(char.IsDigit))
                   select Int32.Parse(new String(digits.ToArray()));
        }

        public Parser<TInput, double> Decimal()
        {
            return from whole in Rep1(Char(char.IsDigit))
                   from point in Str(".")
                   from fract in Rep1(Char(char.IsDigit))
                   select Double.Parse(new String(whole.Concat(point).Concat(fract).ToArray()));
        }
    }

}

