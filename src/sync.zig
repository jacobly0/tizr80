const std = @import("std");

const Sync = @This();
const CEmuCore = @import("cemucore.zig");

const State = packed struct(u32) {
    stopping: bool = false,
    running: bool = false,
    synced: bool = false,
    counter: u29 = 0,

    const Backing = @typeInfo(@This()).Struct.backing_integer.?;
    pub fn from(value: Backing) @This() {
        return @bitCast(@This(), value);
    }
    pub fn all(self: @This()) Backing {
        return @bitCast(Backing, self);
    }
};

mutex: std.Thread.Mutex = .{},
synced: std.Thread.Condition = .{},
wait_synced: std.Thread.Condition = .{},
wait_run: std.Thread.Condition = .{},
state: std.atomic.Atomic(State.Backing) = .{ .value = (State{ .running = true, .counter = 1 }).all() },

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
    return State.from(sync.state.load(.Monotonic));
}
fn setState(sync: *Sync, mask: State) State {
    return State.from(sync.state.fetchOr(mask.all(), .Monotonic));
}
fn clearState(sync: *Sync, mask: State) State {
    return State.from(sync.state.fetchAnd(~mask.all(), .Monotonic));
}
fn incCounter(sync: *Sync) State {
    var state = State.from(sync.state.fetchAdd((State{ .counter = 1 }).all(), .Monotonic));
    state.counter += 1;
    return state;
}
fn decCounter(sync: *Sync, comptime do_unlock: bool) State {
    var state = State.from(sync.state.fetchSub((State{ .counter = 1 }).all(), .Monotonic));
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
    return if (state.running) sync.incCounter() else state;
}
fn wakeMask(sync: *Sync, mask: State) State {
    const state = sync.setState(mask);
    return if (state.running) state else sync.decCounter(false);
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
    return State.from(sync.state.load(.Monotonic)).counter == 0 or sync.loopUnlikely();
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
    sync.leave();
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
