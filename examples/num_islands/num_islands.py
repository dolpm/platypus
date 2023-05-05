# https://leetcode.com/problems/number-of-islands/solutions/3388095/python-solution-beating-100/
class Solution:
    
    def numIslands(self, grid: List[List[str]]) -> int:
          
        if grid is None:
            return 0

        m = len(grid)

        if m == 0:
            return 0
        
        n = len(grid[0])
        if(n==0):
            return 0
        
        ans = 0

        for i in range(m):
            for j in range(n):

                if(grid[i][j]=="1"):
                    ans +=1
                    self.dfs(grid, i,j,m,n)
        
        return ans
    
    def dfs(self, g, i, j, m, n):
        if i<0 or j<0 or i>=m or j>=n or g[i][j] == "0":
            return
        
        g[i][j] = "0"

        self.dfs(g,i-1,j,m,n)
        self.dfs(g,i+1,j,m,n)
        self.dfs(g,i,j-1,m,n)
        self.dfs(g,i,j+1,m,n)