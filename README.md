# 230_project

Final Project for CSE230

**TEAM**

| Name          | PID       |
| ------------- | --------- |
| Sicong Li     | A59002532 |
| Zhenyuan Ding | A59010898 |
| Xin Liu       | A59002628 |
| Wei Peng      | A59002624 |



**Raiders of the Lost Ark**

We are going to design a game application called Raiders of the Lost Ark. In this game, the player would have an adventure on an island. The island is like a maze, which has complex roads and some dead ends. Player’s goal is to find as many treasures on the island as possible. The treasures can be generated at any random place as time goes by. There are some zombies in the maze who are wandering and guarding the treasures and they are also generated randomly. You will be killed once they catch up to you and then the game is over. The zombies are different in their speed and health. To protect yourself, you have a gun, which can shoot the zombies and kill them. There are different kinds of guns with different shoot speed and power.

For every round of the game, a random maze would be generated. The player can press arrow keys to control the character to move in four directions and press space to shoot the gun. Score is measured by the total number of treasures you have collected. Once you are touched by a zombie, the game is over. To make the game more interesting and consistable, Some special guns and some fruit would appear in the maze randomly. The fruit can increase the player's health. And the special gun would have an improvement to the speed and power of the origin gun.In order to finish our project, we will use some third party dependencies like Brick and Graphics. And zombies and players would be represented by a square. Treasures and fruits would be represented by a small square. The whole maze would be represented by line.


**UPDATE**

architecture: 

There are 4 main components in our project - Maze, PlayState, Control, Zombies.
Maze is the shape of the island, which including the logic of generating and drawing map of the island. We use '#' to represent block and ' ' to represent a passable way.

PlayState is the structure of the process of the game, which including current score, current location of the user and information about the treasure.
Control contains the logic about movement and gun shot using keyboard. To be more specific, the arrow key is to move the character (represented by '*') and the space key is to shoot the gun.

Zombies includes the logic about the appearance and movement of the zombies. It includes a random function to put the zombies and the logic of chasing player. Due to the reason that we haven't finish it yet, there could be more details which is missing now.

challenges:

We did meet lots of problem when doing the code. The first challenge is that the document of Brick is not that detailed and there is not much information we could find with Google. Thanks to the demo of tic-tac-toe provided by professor, we implemented the drawing of the maze and the movement of player.
Then, the record of the current state of the game is pretty inconvenient. We had to use parameter-passing to store the information of the map and character location. And there will be much more challenge when we start to code for gun and Zombies.

about the goals:

We believe we can meet most of the goal eventually. Until now, we have already finished the maze and the player. Two major thing left is the zombies and the shooting. We will try our best to finish them. But if we don't have enough time, we may omit the implementation of the shooting.
