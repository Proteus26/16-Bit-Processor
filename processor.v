module bit16 (
    input wire clk,
    input wire rst,
    input wire [15:0] io_in,
    output wire [15:0] io_out,
    output wire [15:0] debug_pc,
    output wire [15:0] debug_reg_data
);
    wire [15:0] pc;
    wire [15:0] instr;
    wire [15:0] alu_result;
    wire [15:0] reg1, reg2;
    wire [15:0] imm;
    wire [4:0] rs1, rs2, rd;
    wire [3:0] alu_op;
    wire reg_write, mem_read, mem_write, branch, jump;
    wire [15:0] mem_out;
    wire [15:0] write_data;
    wire zero_flag;

    assign debug_pc = pc;
    assign debug_reg_data = reg1;

    reg [15:0] pc_reg;
    wire [15:0] pc_next;
    wire pc_branch;
    
    assign pc = pc_reg;
    assign pc_branch = branch & zero_flag;
    assign pc_next = jump ? (pc + imm) : pc_branch ? (pc + imm) : (pc + 16'd2);
    
    always @(posedge clk or posedge rst) begin
        if (rst)
            pc_reg <= 16'd0;
        else
            pc_reg <= pc_next;
    end

    reg [15:0] imem [0:255];
    assign instr = imem[pc[8:1]];

    initial begin
        imem[0] = 16'h1001;
        imem[1] = 16'h2002;
        imem[2] = 16'h0221;
        imem[3] = 16'h4301;
        imem[4] = 16'h5401;
        imem[5] = 16'h6000;

        for (integer i = 6; i < 256; i = i + 1) begin
            imem[i] = 16'h0000;
        end
    end

    instr_decoder decoder (
        .instr(instr),
        .rs1(rs1),
        .rs2(rs2),
        .rd(rd),
        .imm(imm),
        .alu_op(alu_op),
        .reg_write(reg_write),
        .mem_read(mem_read),
        .mem_write(mem_write),
        .branch(branch),
        .jump(jump)
    );

    register_file regfile (
        .clk(clk),
        .rst(rst),
        .rs1(rs1),
        .rs2(rs2),
        .rd(rd),
        .write_data(write_data),
        .reg_write(reg_write),
        .data1(reg1),
        .data2(reg2)
    );

    alu alu_unit (
        .a(reg1),
        .b(reg2),
        .imm(imm),
        .alu_op(alu_op),
        .result(alu_result),
        .zero(zero_flag)
    );

    data_memory dmem (
        .clk(clk),
        .rst(rst),
        .address(alu_result[7:0]),
        .write_data(reg2),
        .mem_read(mem_read),
        .mem_write(mem_write),
        .read_data(mem_out),
        .io_in(io_in),
        .io_out(io_out)
    );

    assign write_data = mem_read ? mem_out : alu_result;

endmodule

module instr_decoder (
    input wire [15:0] instr,
    output reg [4:0] rs1,
    output reg [4:0] rs2,
    output reg [4:0] rd,
    output reg [15:0] imm,
    output reg [3:0] alu_op,
    output reg reg_write,
    output reg mem_read,
    output reg mem_write,
    output reg branch,
    output reg jump
);

    wire [3:0] opcode = instr[3:0];
    wire [3:0] funct3 = instr[7:4];
    wire [7:0] upper_bits = instr[15:8];

    always @(*) begin
        rs1 = 5'd0;
        rs2 = 5'd0;
        rd = 5'd0;
        imm = 16'd0;
        alu_op = 4'd0;
        reg_write = 1'b0;
        mem_read = 1'b0;
        mem_write = 1'b0;
        branch = 1'b0;
        jump = 1'b0;

        case (opcode)
            4'h0: begin
                rs1 = instr[8:4];
                rs2 = instr[13:9];
                rd = instr[15:14];
                alu_op = 4'b0000;
                reg_write = 1'b1;
            end
            4'h1: begin
                rs1 = 5'd0;
                rd = instr[7:4];
                imm = {8'd0, instr[15:8]};
                alu_op = 4'b0001;
                reg_write = 1'b1;
            end
            4'h2: begin
                rs1 = 5'd0;
                rd = instr[7:4];
                imm = {8'd0, instr[15:8]};
                alu_op = 4'b0001;
                reg_write = 1'b1;
            end
            4'h4: begin
                rs1 = instr[8:4];
                rs2 = instr[13:9];
                imm = {8'd0, instr[15:14], 6'd0};
                mem_write = 1'b1;
            end
            4'h5: begin
                rs1 = instr[8:4];
                rd = instr[13:9];
                imm = {8'd0, instr[15:14], 6'd0};
                mem_read = 1'b1;
                reg_write = 1'b1;
            end
            4'h6: begin
                rs1 = instr[8:4];
                rs2 = instr[13:9];
                imm = {8'd0, instr[15:14], 6'd0};
                branch = 1'b1;
                alu_op = 4'b0010;
            end
            default: begin
            end
        endcase
    end
endmodule