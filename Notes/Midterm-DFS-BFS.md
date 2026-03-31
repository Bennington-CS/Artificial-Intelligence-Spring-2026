# AI Quarterterm — Sample Answer

## Search & Pathfinding: BFS / DFS

---

## 1. Definition & Purpose

BFS (Breadth-First Search) and DFS (Depth-First Search) are **uninformed search algorithms** used for pathfinding and graph traversal. "Uninformed" means they have no heuristic—no clue about which direction is promising—so they explore the search space mechanically.

BFS uses a **queue (FIFO)**. It explores every node at depth *d* before touching anything at depth *d + 1*. The result is a layer-by-layer expansion outward from the start. DFS uses a **stack (LIFO)**, which means it dives as deep as possible along one branch before backtracking. Both find *a* path from start to goal if one exists, but BFS guarantees the **shortest path** (fewest edges); DFS does not.

**Real-world scenario:** BFS is a natural fit for a subway system where you want the route with the fewest transfers between two stations. Every connection has equal cost, and BFS's layer-by-layer search finds the minimum-step path by construction.

---

## 4. Worked Example / Trace

Consider a 3×3 grid. Start = (2, 2), Goal = (0, 0), single wall at (1, 1). Neighbours are generated in the order **up, down, left, right**, and we skip anything out of bounds, blocked, or already visited.

```
         col 0   col 1   col 2
Row 0:    ☆       .       .          ☆ = goal
Row 1:    .       █       .          █ = wall
Row 2:    .       .       #          # = start
```

### BFS Trace (Queue / FIFO)

We dequeue from the front and enqueue new neighbours at the back. Nodes are marked visited when dequeued.

| Step | Dequeue | Goal? | Valid Unvisited Neighbours | Queue (after) | Visited Set |
|------|---------|-------|---------------------------|---------------|-------------|
| 0 | — | — | — | [(2,2)] | { } |
| 1 | (2,2) | No | (1,2), (2,1) | [(1,2), (2,1)] | {(2,2)} |
| 2 | (1,2) | No | (0,2) | [(2,1), (0,2)] | {(2,2), (1,2)} |
| 3 | (2,1) | No | (2,0) | [(0,2), (2,0)] | {…, (2,1)} |
| 4 | (0,2) | No | (0,1) | [(2,0), (0,1)] | {…, (0,2)} |
| 5 | (2,0) | No | (1,0) | [(0,1), (1,0)] | {…, (2,0)} |
| 6 | (0,1) | No | (0,0) | [(1,0), (0,0)] | {…, (0,1)} |
| 7 | (1,0) | No | (0,0)—already in queue | [(0,0), (0,0)] | {…, (1,0)} |
| 8 | (0,0) | Yes! | — | — | {…, (0,0)} |

**Path reconstruction** (following parent pointers back from goal):

```
(2,2) → (1,2) → (0,2) → (0,1) → (0,0)
```

That's **4 edges**. BFS guarantees this is the shortest path. Notice that BFS explored the grid in expanding layers: it visited everything 1 step away, then 2 steps, and so on. By the time it found the goal, there was no shorter route it hadn't already checked.

### DFS Trace (Stack / LIFO)

Same grid, but now we pop from the top of the stack. Neighbours are pushed in order (up, down, left, right), so the **rightmost/last-pushed** neighbour gets processed first.

| Step | Pop | Goal? | Valid Unvisited Neighbours | Stack (after, top→right) | Visited Set |
|------|-----|-------|---------------------------|--------------------------|-------------|
| 0 | — | — | — | [(2,2)] | { } |
| 1 | (2,2) | No | (1,2), (2,1) | [(1,2), (2,1)] | {(2,2)} |
| 2 | (2,1) | No | (2,0) | [(1,2), (2,0)] | {(2,2), (2,1)} |
| 3 | (2,0) | No | (1,0) | [(1,2), (1,0)] | {…, (2,0)} |
| 4 | (1,0) | No | (0,0) | [(1,2), (0,0)] | {…, (1,0)} |
| 5 | (0,0) | Yes! | — | — | {…, (0,0)} |

**Path reconstruction:**

```
(2,2) → (2,1) → (2,0) → (1,0) → (0,0)
```

Also **4 edges**; but that's a coincidence. DFS happened to find an equally short path here because the grid is small and the wall doesn't create long dead ends. On a larger grid with corridors or dead-end branches, DFS could easily commit to a much longer detour before backtracking. The crucial difference: DFS went *left then down* (hugging the bottom of the grid), while BFS went *up then across the top*. The LIFO vs. FIFO structure drives entirely different exploration patterns.

Also: DFS only visited **5 nodes** to find the goal, while BFS visited **8** (the whole grid minus the wall). DFS can sometimes get lucky and reach the goal quickly if the stack order happens to push it in a good direction.

---

## 5. Performance Characteristics

**Memory use** is the biggest practical difference between the two. BFS stores all nodes at the current depth level in its queue. In a grid with branching factor *b* and solution depth *d*, that frontier can grow as large as *bᵈ*. For a wide-open grid, that means BFS might have an entire row (or more) of nodes queued up at once. DFS, by contrast, only stores nodes along the current path plus their unexplored siblings. Its memory is proportional to the *depth* of the search, not the breadth. On a large maze, this matters a lot.

**Exploration pattern and goal location** also affect the number of steps. BFS has to chew through every depth layer in order, so if the goal is far away, it does a lot of work at every layer along the way. That said, it never wastes time exploring deeper than necessary. DFS can reach deep nodes quickly (great if the goal happens to be deep) but it can also charge down a very long dead-end branch and backtrack in costly ways. In the worst case, DFS explores the entire graph before finding a goal that was only a few steps from the start.

---

## 6. Comparative Analysis

The natural comparison is **A\***, which is also a pathfinding algorithm but belongs to the *informed* search family.

**Advantage of BFS/DFS over A\*:** BFS and DFS need zero domain knowledge. You don't have to design a heuristic; you just give them a graph and they go. A\* requires a heuristic function (like Manhattan distance), and if you pick one that's *inadmissible*—i.e., it overestimates the cost to the goal—A\* can return suboptimal paths. BFS avoids this entire class of design decisions and still guarantees the shortest path.

**Disadvantage of BFS/DFS compared to A\*:** Being uninformed means they can't prioritize promising directions. On the 5×5 map from class, A\* uses *f = g + h* to focus its search toward the goal, exploring far fewer nodes than BFS would. BFS expands in every direction equally, even directions that are obviously wrong (like searching south when the goal is north). DFS might go the right way by luck, but it also might not. A\* with a good heuristic systematically avoids this wasted work.
