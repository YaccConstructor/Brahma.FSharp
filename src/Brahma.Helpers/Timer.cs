#region License and Copyright Notice
// Copyright (c) 2010 Ananth B.
// All rights reserved.
// 
// The contents of this file are made available under the terms of the
// Eclipse Public License v1.0 (the "License") which accompanies this
// distribution, and is available at the following URL:
// http://www.opensource.org/licenses/eclipse-1.0.php
// 
// Software distributed under the License is distributed on an "AS IS" basis,
// WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
// the specific language governing rights and limitations under the License.
// 
// By using this software in any fashion, you are agreeing to be bound by the
// terms of the License.
#endregion

using System;
using System.Collections;
using System.Collections.Generic;
using System.Runtime.InteropServices;

namespace Brahma.Helpers
{
    internal static class NativeMethods
    {
        [DllImport("Kernel32.dll")]
        public static extern bool QueryPerformanceCounter(out long count);

        [DllImport("Kernel32.dll")]
        public static extern bool QueryPerformanceFrequency(out long frequency);
    }

    public sealed class Timer<TMarker> : IEnumerable<Tuple<TMarker, double>>
    {
        private readonly long _frequency;
        private readonly Stack<long> _startTimes = new Stack<long>();
        private readonly List<Tuple<TMarker, double>> _laps = new List<Tuple<TMarker, double>>();

        public static readonly Timer<TMarker> Global = new Timer<TMarker>();

        public Timer()
        {
            if (!NativeMethods.QueryPerformanceFrequency(out _frequency))
                throw new NotSupportedException("High performance timers are not supported on this system");
        }

        public void Start()
        {
            long startTime;
            NativeMethods.QueryPerformanceCounter(out startTime);
            _startTimes.Push(startTime);
        }

        public double Stop()
        {
            if (_startTimes.Count == 0)
                throw new InvalidOperationException("Unmatched starts/stops");

            long stopTime;
            NativeMethods.QueryPerformanceCounter(out stopTime);

            return (double)(stopTime - _startTimes.Pop()) / _frequency;
        }

        public void Lap(TMarker marker, bool stop = false)
        {
            _laps.Add(new Tuple<TMarker, double>(marker, Stop()));
            if (!stop)
                Start();
        }

        public void Reset()
        {
            _startTimes.Clear();
            _laps.Clear();
        }

        #region IEnumerable<Tuple<TMarker,double>> Members

        public IEnumerator<Tuple<TMarker, double>> GetEnumerator()
        {
            return _laps.GetEnumerator();
        }

        #endregion

        #region IEnumerable Members

        IEnumerator IEnumerable.GetEnumerator()
        {
            return GetEnumerator();
        }

        #endregion

        public double Total(TMarker marker)
        {
            double total = 0.0;

            foreach (var lap in _laps)
                if (lap.Item1.Equals(marker))
                    total += lap.Item2;

            return total;
        }

        public double Average(TMarker marker)
        {
            double total = 0.0;
            int count = 0;

            foreach (var lap in _laps)
                if (lap.Item1.Equals(marker))
                {
                    total += lap.Item2;
                    count++;
                }

            return total / count;
        }

        public double Max(TMarker marker)
        {
            double max = double.MinValue;

            foreach (var lap in _laps)
                if (lap.Item1.Equals(marker))
                    max = Math.Max(max, lap.Item2);

            return max;
        }

        public double Min(TMarker marker)
        {
            double min = double.MaxValue;

            foreach (var lap in _laps)
                if (lap.Item1.Equals(marker))
                    min = Math.Min(min, lap.Item2);

            return min;
        }
    }
}