using System;
using System.Collections.Generic;
using System.Text;

namespace GSharpTools
{
    public static class StringList 
    {
        private static List<string> Sorted(this IEnumerable<string> input)
        {
            List<string> result = new List<string>();
            foreach (string item in input)
            {
                result.Add(item);
            }
            result.Sort();
            return result;
        }

        public static void Filter(this List<string> source, IEnumerable<string> itemsToRemove, StringComparison sc)
        {
            for( int i = 0; i < source.Count; ++i )                
            {
                string a = source[i];
                foreach(string b in itemsToRemove)
                {
                    if( a.Equals(b, sc))
                    {
                        source.RemoveAt(i);
                    }
                }
            }
        }

        public static bool ExistsInSubset(this IEnumerable<string> items, int size, string s, StringComparison sc)
        {
            int nCount = 0;
            foreach (string n in items)
            {
                if (nCount++ == size)
                    break;

                if (s.Equals(n, sc))
                    return true;
            }
            return false;
        }

        public static void MakeUnique(this List<string> items, StringComparison sc)
        {
            List<int> IndicesToRemove = new List<int>();

            int index = 0;
            foreach (string s in items)
            {
                if (index > 0)
                {
                    if (ExistsInSubset(items, index, s, sc))
                    {
                        IndicesToRemove.Add(index);
                    }
                }
                ++index;
            }

            if (IndicesToRemove.Count > 0)
            {
                for (index = IndicesToRemove.Count - 1; index >= 0; --index)
                {
                    items.RemoveAt(IndicesToRemove[index]);
                }
            }
        }
    }
}
