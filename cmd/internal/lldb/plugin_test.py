# Copyright (c) 2026 The XGo Authors (xgo.dev). All rights reserved.
# Licensed under the Apache License, Version 2.0.

"""Type spelling regressions that do not require an LLDB installation."""

import importlib.util
from pathlib import Path
import sys
from types import ModuleType, SimpleNamespace
import unittest
from unittest.mock import MagicMock, Mock


lldb = ModuleType("lldb")
lldb.__getattr__ = lambda name: type(name, (), {})
sys.modules["lldb"] = lldb
spec = importlib.util.spec_from_file_location(
    "llgo_plugin", Path(__file__).with_name("llgo_plugin.py"))
plugin = importlib.util.module_from_spec(spec)
sys.modules[spec.name] = plugin
spec.loader.exec_module(plugin)


class ValueType:
    def __init__(self, name, typedef=False, pointee=None):
        self.name, self.typedef, self.pointee = name, typedef, pointee

    def GetName(self):
        return self.name

    def IsPointerType(self):
        return self.pointee is not None

    def GetPointeeType(self):
        return self.pointee

    def IsTypedefType(self):
        return self.typedef


class TypeNameTests(unittest.TestCase):
    def test_longest_c_spelling_wins(self):
        for spelling, expected in (
            ("long long", "int64"), ("unsigned long long", "uint64"),
            ("long long *", "*int64"), ("unsigned long long *", "*uint64"),
            ("long", "int"), ("unsigned long", "uint"),
        ):
            with self.subTest(spelling=spelling):
                self.assertEqual(plugin.map_type_name(spelling), expected)

    def test_go_typedef_names_are_preserved(self):
        for name in ("int", "int64", "uint", "uint64", "main.NamedInt"):
            go_type = ValueType(name, typedef=True)
            self.assertEqual(plugin.go_type_name(go_type), name)
            self.assertEqual(plugin.go_type_name(ValueType(name + " *", pointee=go_type)), "*" + name)
        self.assertEqual(plugin.go_type_name(ValueType("int")), "int32")


class CollectorSignalTests(unittest.TestCase):
    def test_only_collector_signals_are_delivered_and_continued(self):
        lldb.eStateStopped = 5
        lldb.eStopReasonSignal = 5
        lldb.eStopReasonBreakpoint = 3
        lldb.eStopReasonException = 6
        fixture_path = Path(__file__).parents[2] / "llgo" / "lldbtest" / "test.py"
        fixture_spec = importlib.util.spec_from_file_location("lldb_fixture", fixture_path)
        fixture = importlib.util.module_from_spec(fixture_spec)
        sys.modules[fixture_spec.name] = fixture
        fixture_spec.loader.exec_module(fixture)
        debugger = fixture.LLDBDebugger.__new__(fixture.LLDBDebugger)
        debugger.target = SimpleNamespace(GetTriple=lambda: "aarch64-unknown-linux-gnu")
        process = debugger.process = MagicMock()
        process.GetState.return_value = lldb.eStateStopped
        thread = process.GetSelectedThread.return_value
        process.__iter__.return_value = [thread]
        thread.GetStopReason.return_value = lldb.eStopReasonSignal
        signals = process.GetUnixSignals.return_value
        signals.GetSignalNumberFromName.side_effect = {"SIGPWR": 30, "SIGXCPU": 24}.get
        for signal_number in (30, 24):
            with self.subTest(signal=signal_number):
                thread.GetStopReason.return_value = lldb.eStopReasonSignal
                thread.GetStopReasonDataAtIndex.return_value = signal_number
                process.Continue.side_effect = lambda: setattr(
                    thread.GetStopReason, "return_value", lldb.eStopReasonBreakpoint)
                process.Continue.reset_mock()
                debugger.continue_gc_signals()
                process.Continue.assert_called_once()
                signals.SetShouldStop.assert_any_call(signal_number, False)
                signals.SetShouldSuppress.assert_any_call(signal_number, False)
        # SIGSEGV must remain a visible failure, even after GC signal setup.
        thread.GetStopReason.return_value = lldb.eStopReasonSignal
        thread.GetStopReasonDataAtIndex.return_value = 11
        process.Continue.reset_mock()
        debugger.continue_gc_signals()
        process.Continue.assert_not_called()
        # A selected GC stop cannot resume past another thread's real fault.
        other_fault = Mock()
        other_fault.GetStopReason.return_value = lldb.eStopReasonSignal
        other_fault.GetStopReasonDataAtIndex.return_value = 11
        process.__iter__.return_value = [thread, other_fault]
        thread.GetStopReasonDataAtIndex.return_value = 30
        debugger.continue_gc_signals()
        process.Continue.assert_not_called()
        thread.GetStopReasonDataAtIndex.return_value = 11
        # A second thread at the right source line cannot hide a real fault.
        debugger.breakpoint_id = 1
        breakpoint_thread = Mock()
        breakpoint_thread.GetStopReason.return_value = lldb.eStopReasonBreakpoint
        breakpoint_thread.GetStopDescription.return_value = "breakpoint 1.1"
        breakpoint_thread.GetStopReasonDataCount.return_value = 2
        breakpoint_thread.GetStopReasonDataAtIndex.side_effect = [1, 1]
        thread.GetStopDescription.return_value = "signal SIGSEGV"
        with self.assertRaisesRegex(fixture.LLDBTestException, "SIGSEGV"):
            debugger.breakpoint_threads([thread, breakpoint_thread])
        self.assertEqual(debugger.breakpoint_threads([breakpoint_thread]), [breakpoint_thread])
        debugger.target = Mock()
        debugger.target.GetTriple.return_value = "i686-pc-windows-msvc"
        debugger.target.FindBreakpointByID.return_value.GetHitCount.return_value = 1
        thread.GetStopReason.return_value = lldb.eStopReasonException
        thread.GetStopDescription.return_value = "Exception 0x80000003 at address 0x1234"
        self.assertEqual(debugger.breakpoint_threads([thread]), [thread])
        thread.GetStopDescription.return_value = "Exception 0xc0000005 at address 0x1234"
        with self.assertRaisesRegex(fixture.LLDBTestException, "0xc0000005"):
            debugger.breakpoint_threads([thread])


if __name__ == "__main__":
    unittest.main()
