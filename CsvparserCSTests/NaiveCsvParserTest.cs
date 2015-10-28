using CsvParserCS;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace CsvparserCSTests
{
    [TestClass]
    public class NaiveCsvParserTest
    {
        [TestMethod]
        public void SimpleCsvLineTest()
        {
            var input = "1, 2 ,   3  ";

            var expected = new[] {new[] {"1", "2", "3"}};

            var result = NaiveCsvParser.ParseCSV(input);
            
            Assert.AreEqual(1, result.Length);
            for (int i = 0; i < expected.Length; i++)
            {
                CollectionAssert.AreEqual(expected[i], result[i]);
            }
        }

        [TestMethod]
        public void TwoSimpleCsvLineTest()
        {
            var input = @"1, 2 ,   3  
A    ,B, C, D, E";

            var expected = new[]
            {
                new[] {"1", "2", "3"},
                new[] {"A", "B", "C", "D", "E"},
            };

            var result = NaiveCsvParser.ParseCSV(input);
            
            Assert.AreEqual(2, result.Length);
            for (int i = 0; i < expected.Length; i++)
            {
                CollectionAssert.AreEqual(expected[i], result[i]);
            }
        }


        [TestMethod]
        public void ComplexCsvLineTest()
        {
            var input = @"A1,B1,""C1,+comma"",D1
,B2,""line 1
line 2"",D2
,,C3,""D3,+comma""
,,,D4 space";

            var expected = new[]
            {
                new[] {"A1", "B1", "C1,+comma", "D1"},
                new[] {"", "B2", "line 1\nline 2", "D2"},
                new[] {"", "", "C3", "D3,+comma"},
                new[] {"", "", "", "D4 space"},
            };

            var result = NaiveCsvParser.ParseCSV(input);
            
            Assert.AreEqual(4, result.Length);
            for (int i = 0; i < expected.Length; i++)
            {
                CollectionAssert.AreEqual(expected[i], result[i]);
            }
        }
    }
}
