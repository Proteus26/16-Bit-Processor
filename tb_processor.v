module tb_bit16;
    reg clk, rst;
    reg [15:0] io_in;
    wire [15:0] io_out;
    wire [15:0] debug_pc;
    wire [15:0] debug_reg_data;

    bit16 uut (
        .clk(clk),
        .rst(rst),
        .io_in(io_in),
        .io_out(io_out),
        .debug_pc(debug_pc),
        .debug_reg_data(debug_reg_data)
    );

    initial begin
        clk = 0;
        forever #5 clk = ~clk;
    end

    initial begin
        rst = 1;
        io_in = 16'h1234;
        #20 rst = 0;
        
        #200 $finish;
    end

    initial begin
        $monitor("Time: %0t | PC: %h | io_out: %h | REG_DATA: %h", $time, debug_pc, io_out, debug_reg_data);
    end

endmodule