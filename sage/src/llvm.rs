extern crate llvm_sys as llvm;

use std::ptr;

pub unsafe fn llvm_test()
{
    let context = llvm::core::LLVMContextCreate();
    let module = llvm::core::LLVMModuleCreateWithName(b"nop\0".as_ptr() as *const _);
    let builder = llvm::core::LLVMCreateBuilderInContext(context);

    let void = llvm::core::LLVMVoidTypeInContext(context);
    let function_type = llvm::core::LLVMFunctionType(void, ptr::null_mut(), 0, 0);
    let function = llvm
        ::core
        ::LLVMAddFunction(module, b"nop\0".as_ptr() as *const _, function_type);

    let bb = llvm::core::LLVMAppendBasicBlockInContext(context, function, b"entry\0".as_ptr() as *const _);
    llvm::core::LLVMPositionBuilderAtEnd(builder, bb);

    llvm::core::LLVMBuildRetVoid(builder);

    llvm::core::LLVMDumpModule(module);

    llvm::core::LLVMDisposeBuilder(builder);
    llvm::core::LLVMDisposeModule(module);
    llvm::core::LLVMContextDispose(context);
}