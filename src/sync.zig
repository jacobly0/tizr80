const std = @import("std");

const CEmuCore = @import("cemucore.zig");
const Sync = @This();
const util = @import("util.zig");

const State = packed struct(u32) {
    stopping: bool = false,
    running: bool = false,
    synced: bool = false,
    counter: u29 = 0,
};

mutex: std.Thread.Mutex = .{},
synced: std.Thread.Condition = .{},
wait_synced: std.Thread.Condition = .{},
wait_run: std.Thread.Condition = .{},
state: std.atomic.Atomic(util.Backing(State)) = .{ .value = util.toBacking(State{ .counter = 1 }) },

pub fn init(sync: *Sync) !void {
    sync.* = .{};
    sync.lock();
}
pub fn deinit(sync: *Sync) void {
    _ = sync;
}

fn lock(sync: *Sync) void {
    sync.mutex.lock();
}
fn unlock(sync: *Sync) void {
    sync.mutex.unlock();
}

fn getState(sync: *Sync) State {
    return util.fromBacking(State, sync.state.load(.Monotonic));
}
fn setState(sync: *Sync, mask: State) State {
    return util.fromBacking(State, sync.state.fetchOr(util.toBacking(mask), .Monotonic));
}
fn clearState(sync: *Sync, mask: State) State {
    return util.fromBacking(State, sync.state.fetchAnd(~util.toBacking(mask), .Monotonic));
}
fn incCounter(sync: *Sync) State {
    var state = util.fromBacking(State, sync.state.fetchAdd(util.toBacking(State{ .counter = 1 }), .Monotonic));
    state.counter += 1;
    return state;
}
fn decCounter(sync: *Sync, comptime do_unlock: bool) State {
    var state = util.fromBacking(State, sync.state.fetchSub(util.toBacking(State{ .counter = 1 }), .Monotonic));
    state.counter -= 1;
    if (do_unlock) sync.unlock();
    (if (state.counter == 0) sync.synced else sync.wait_synced).signal();
    return state;
}

fn waitSynced(sync: *Sync) State {
    var state = sync.incCounter();
    while (!state.synced) {
        sync.wait_synced.wait(&sync.mutex);
        state = sync.getState();
    }
    return state;
}
fn waitRun(sync: *Sync) State {
    var state = sync.decCounter(false);
    while (state.counter != 0 or state.synced) {
        sync.wait_run.wait();
        state = sync.getState();
    }
    return state;
}

fn sleepMask(sync: *Sync, mask: State) State {
    const state = sync.clearState(mask);
    if (state.running) _ = sync.incCounter();
    return state;
}
fn wakeMask(sync: *Sync, mask: State) State {
    const state = sync.setState(mask);
    if (!state.running) _ = sync.decCounter(false);
    return state;
}

pub fn leaveMask(sync: *Sync, mask: State) bool {
    const stopping = sync.clearState(mask).stopping;
    sync.unlock();
    sync.wait_run.broadcast();
    return !stopping;
}

fn loopUnlikely(sync: *Sync) bool {
    @setCold(true);
    sync.lock();
    var state = sync.setState(.{ .synced = true });
    while (state.counter != 0 and !state.stopping) {
        sync.wait_synced.signal();
        sync.synced.wait(&sync.mutex);
        state = sync.getState();
    }
    return sync.leaveMask(.{ .stopping = true, .synced = true });
}
pub fn loop(sync: *Sync) bool {
    return util.fromBacking(State, sync.state.load(.Monotonic)).counter == 0 or sync.loopUnlikely();
}

pub fn delay(sync: *Sync, delay_ns: u64) bool {
    var timer = std.time.Timer.start() catch return false;
    sync.lock();
    var stopping = sync.setState(.{ .synced = true }).stopping;
    while (!stopping) {
        const remaining = delay_ns -| timer.read();
        if (remaining == 0) break;
        sync.wait_synced.signal();
        sync.synced.timedWait(&sync.mutex, remaining) catch break;
        stopping = sync.getState().stopping;
    }
    return sync.leaveMask(.{ .synced = true });
}

pub fn sleep(sync: *Sync) bool {
    return sync.sleepMask(.{ .running = true }).running;
}
pub fn wake(sync: *Sync) bool {
    return !sync.wakeMask(.{ .running = true }).running;
}
pub fn enter(sync: *Sync) void {
    sync.lock();
    _ = sync.waitSynced();
}
pub fn run(sync: *Sync) void {
    _ = sync.waitRun();
    _ = sync.waitSynced();
}
pub fn leave(sync: *Sync) void {
    _ = sync.decCounter(true);
}
pub fn runLeave(sync: *Sync) void {
    _ = sync.waitRun();
    sync.unlock();
}

pub fn start(sync: *Sync) void {
    sync.unlock();
}
pub fn stop(sync: *Sync) void {
    sync.enter();
    _ = sync.wakeMask(.{ .stopping = true, .running = true });
    var stopping = true;
    while (stopping) {
        sync.synced.signal();
        sync.wait_run.wait(&sync.mutex);
        stopping = sync.getState().stopping;
    }
    sync.leave();
}
