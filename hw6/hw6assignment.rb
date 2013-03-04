# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.
class MyTetris < Tetris
  # your enhancements here
  def key_bindings
    super
    @root.bind("u", proc {@board.rotate_inverse})
    @root.bind("c", proc {@board.cheat})
  end

  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

end

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here

  # your enhancements here
  def self.next_piece (board)
    if board.cheat?
      MyPiece.new([[[0, 0]]], board)
    else
      MyPiece.new(All_My_Pieces.sample, board)
    end
  end

  All_My_Pieces = [
               [[[0, 0], [1, 0], [0, 1], [1, 1]]],  # square (only needs one)
               rotations([[0, 0], [-1, 0], [1, 0], [0, -1]]), # T
               [[[0, 0], [-1, 0], [1, 0], [2, 0]], # long (only needs two)
               [[0, 0], [0, -1], [0, 1], [0, 2]]],
               rotations([[0, 0], [0, -1], [0, 1], [1, 1]]), # L
               rotations([[0, 0], [0, -1], [0, 1], [-1, 1]]), # inverted L
               rotations([[0, 0], [-1, 0], [0, -1], [1, -1]]), # S
               rotations([[0, 0], [1, 0], [0, -1], [-1, -1]]), # Z
               rotations([[0, 0], [1, 0], [0, -1], [1, -1], [2, -1]]),
               rotations([[-2, 0], [-1, 0], [0, 0], [1, 0], [2, 0]]),
               rotations([[0, 0], [0, -1], [1, 0]])
  ]
end

class MyBoard < Board
  # your enhancements here
  def initialize (game)
    super(game)
    @current_block = MyPiece.next_piece(self)
    @cheat_next = false
  end

  def rotate_inverse
    if !game_over? and @game.is_running?
      @current_block.move(0, 0, 2)
    end
    draw
  end

  def cheat?
    @cheat_next
  end

  def next_piece
    @current_block = MyPiece.next_piece(self)
    @cheat_next = false
    @current_pos = nil
  end

  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    # fix the "bug" in provides code
    (0..(locations.size-1)).each{|index| 
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] = 
      @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end

  def cheat
    if @score >= 100 and !@cheat_next
      @cheat_next = true
      @score -= 100
    end
  end
end









