using System.Linq;

namespace CsvParserCS
{
    public static class NaiveCsvParser
    {
        public static string[][] ParseCSV(string csvText)
        {
            if (string.IsNullOrWhiteSpace(csvText))
            {
                return new string[0][];
            }

            var result = csvText.Split('\n')
                .Select( line => 
                    line.Trim().Split(',')
                        .Select( field => field.Trim())
                        .ToArray())
                .ToArray();

            return result;
        }
    }
}
