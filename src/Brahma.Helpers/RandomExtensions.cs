using System;

namespace Brahma.Helpers
{
    public static class RandomExtensions
    {
        public static float Random(this Random random, float low, float high)
        {
            var lerp = (float)random.NextDouble();
            return (1f - lerp) * low + lerp * high;
        }
    }
}
