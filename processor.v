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

module register_file (
    input wire clk,
    input wire rst,
    input wire [4:0] rs1,
    input wire [4:0] rs2,
    input wire [4:0] rd,
    input wire [15:0] write_data,
    input wire reg_write,
    output wire [15:0] data1,
    output wire [15:0] data2
);

    reg [15:0] registers [0:31];
    integer i;

    always @(posedge clk or posedge rst) begin
        if (rst) begin
            for (i = 0; i < 32; i = i + 1) begin
                registers[i] <= 16'd0;
            end
        end else if (reg_write && rd != 5'd0) begin
            registers[rd] <= write_data;
        end
    end

    assign data1 = (rs1 == 5'd0) ? 16'd0 : registers[rs1];
    assign data2 = (rs2 == 5'd0) ? 16'd0 : registers[rs2];

endmodule

module alu (
    input wire [15:0] a,
    input wire [15:0] b,
    input wire [15:0] imm,
    input wire [3:0] alu_op,
    output reg [15:0] result,
    output wire zero
);

    always @(*) begin
        case (alu_op)
            4'b0000: result = a + b;
            4'b0001: result = a + imm;
            4'b0010: result = a - b;
            4'b0011: result = a & b;
            4'b0100: result = a | b;
            4'b0101: result = a ^ b;
            4'b0110: result = a << b[3:0];
            4'b0111: result = a >> b[3:0];
            default: result = 16'd0;
        endcase
    end

    assign zero = (result == 16'd0);

endmodule

module data_memory (
    input wire clk,
    input wire rst,
    input wire [7:0] address,
    input wire [15:0] write_data,
    input wire mem_read,
    input wire mem_write,
    output reg [15:0] read_data,
    input wire [15:0] io_in,
    output reg [15:0] io_out
);

    reg [15:0] memory [0:255];
    integer i;

    always @(posedge clk or posedge rst) begin
        if (rst) begin
            for (i = 0; i < 256; i = i + 1) begin
                memory[i] <= 16'd0;
            end
            io_out <= 16'd0;
        end else begin
            if (mem_write) begin
                if (address == 8'hFF) begin
                    io_out <= write_data;
                end else begin
                    memory[address] <= write_data;
                end
            end
        end
    end

    always @(*) begin
        if (mem_read) begin
            if (address == 8'hFE) begin
                read_data = io_in;
            end else if (address == 8'hFF) begin
                read_data = io_out;
            end else begin
                read_data = memory[address];
            end
        end else begin
            read_data = 16'd0;
        end
    end

endmodule