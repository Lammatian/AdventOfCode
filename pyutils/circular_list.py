class Node:
    def __init__(self, val):
        self.val = val
        self.n = self
        self.p = self

    def next(self):
        return self.n

    def prev(self):
        return self.p


class CircularList:
    """
    List that wraps around. Keeps track of the current element as the head
    """
    def __init__(self):
        self.head = Node(0)

    def forward(self, n: int):
        """Move n steps clockwise"""
        for _ in range(n):
            self.head = self.head.next()

    def back(self, n: int):
        """Move n steps counter-clockwise"""
        for _ in range(n):
            self.head = self.head.prev()

    def insert(self, val: int):
        """Insert node with value `val` as the new head, old head becomes the second node"""
        new_node = Node(val)
        before = self.head.p
        before.n = new_node
        new_node.p = before
        new_node.n = self.head
        self.head.p = new_node
        self.head = new_node

    def remove(self) -> int:
        """Remove the current head, makes the next element new head"""
        self.head.p.n = self.head.n
        self.head.n.p = self.head.p
        val = self.head.val
        self.head = self.head.n
        return val

