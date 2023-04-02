; ModuleID = 'list.c'
source_filename = "list.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct.List = type { i32, i32, i8* }

@.str = private unnamed_addr constant [18 x i8] c"index < v->length\00", align 1
@.str.1 = private unnamed_addr constant [7 x i8] c"list.c\00", align 1
@__PRETTY_FUNCTION__.Vector_get = private unnamed_addr constant [37 x i8] c"void *Vector_get(struct List *, int)\00", align 1
@.str.2 = private unnamed_addr constant [14 x i8] c"v->length > 0\00", align 1
@__PRETTY_FUNCTION__.Vector_pop = private unnamed_addr constant [32 x i8] c"void *Vector_pop(struct List *)\00", align 1
@.str.3 = private unnamed_addr constant [18 x i8] c"pooped value: %d\0A\00", align 1

; Function Attrs: noinline nounwind optnone uwtable
define dso_local %struct.List* @Vector_alloc() #0 {
  %1 = alloca %struct.List*, align 8
  %2 = call noalias i8* @malloc(i64 noundef 16) #5
  %3 = bitcast i8* %2 to %struct.List*
  store %struct.List* %3, %struct.List** %1, align 8
  %4 = load %struct.List*, %struct.List** %1, align 8
  %5 = getelementptr inbounds %struct.List, %struct.List* %4, i32 0, i32 1
  store i32 10, i32* %5, align 4
  %6 = load %struct.List*, %struct.List** %1, align 8
  %7 = getelementptr inbounds %struct.List, %struct.List* %6, i32 0, i32 0
  store i32 0, i32* %7, align 8
  %8 = load %struct.List*, %struct.List** %1, align 8
  %9 = getelementptr inbounds %struct.List, %struct.List* %8, i32 0, i32 1
  %10 = load i32, i32* %9, align 4
  %11 = sext i32 %10 to i64
  %12 = mul i64 %11, 8
  %13 = call noalias i8* @malloc(i64 noundef %12) #5
  %14 = load %struct.List*, %struct.List** %1, align 8
  %15 = getelementptr inbounds %struct.List, %struct.List* %14, i32 0, i32 2
  store i8* %13, i8** %15, align 8
  %16 = load %struct.List*, %struct.List** %1, align 8
  ret %struct.List* %16
}

; Function Attrs: nounwind
declare noalias i8* @malloc(i64 noundef) #1

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i8* @Vector_get(%struct.List* noundef %0, i32 noundef %1) #0 {
  %3 = alloca %struct.List*, align 8
  %4 = alloca i32, align 4
  store %struct.List* %0, %struct.List** %3, align 8
  store i32 %1, i32* %4, align 4
  %5 = load i32, i32* %4, align 4
  %6 = load %struct.List*, %struct.List** %3, align 8
  %7 = getelementptr inbounds %struct.List, %struct.List* %6, i32 0, i32 0
  %8 = load i32, i32* %7, align 8
  %9 = icmp slt i32 %5, %8
  br i1 %9, label %10, label %11

10:                                               ; preds = %2
  br label %12

11:                                               ; preds = %2
  call void @__assert_fail(i8* noundef getelementptr inbounds ([18 x i8], [18 x i8]* @.str, i64 0, i64 0), i8* noundef getelementptr inbounds ([7 x i8], [7 x i8]* @.str.1, i64 0, i64 0), i32 noundef 23, i8* noundef getelementptr inbounds ([37 x i8], [37 x i8]* @__PRETTY_FUNCTION__.Vector_get, i64 0, i64 0)) #6
  unreachable

12:                                               ; preds = %10
  %13 = load %struct.List*, %struct.List** %3, align 8
  %14 = getelementptr inbounds %struct.List, %struct.List* %13, i32 0, i32 2
  %15 = load i8*, i8** %14, align 8
  %16 = load i32, i32* %4, align 4
  %17 = sext i32 %16 to i64
  %18 = mul i64 %17, 8
  %19 = getelementptr i8, i8* %15, i64 %18
  %20 = bitcast i8* %19 to i8**
  %21 = load i8*, i8** %20, align 8
  ret i8* %21
}

; Function Attrs: noreturn nounwind
declare void @__assert_fail(i8* noundef, i8* noundef, i32 noundef, i8* noundef) #2

; Function Attrs: noinline nounwind optnone uwtable
define dso_local void @Vector_free(%struct.List* noundef %0) #0 {
  %2 = alloca %struct.List*, align 8
  %3 = alloca i32, align 4
  %4 = alloca i8*, align 8
  store %struct.List* %0, %struct.List** %2, align 8
  store i32 0, i32* %3, align 4
  br label %5

5:                                                ; preds = %22, %1
  %6 = load i32, i32* %3, align 4
  %7 = load %struct.List*, %struct.List** %2, align 8
  %8 = getelementptr inbounds %struct.List, %struct.List* %7, i32 0, i32 0
  %9 = load i32, i32* %8, align 8
  %10 = icmp slt i32 %6, %9
  br i1 %10, label %11, label %25

11:                                               ; preds = %5
  %12 = load %struct.List*, %struct.List** %2, align 8
  %13 = getelementptr inbounds %struct.List, %struct.List* %12, i32 0, i32 2
  %14 = load i8*, i8** %13, align 8
  %15 = load i32, i32* %3, align 4
  %16 = sext i32 %15 to i64
  %17 = mul i64 %16, 8
  %18 = getelementptr i8, i8* %14, i64 %17
  %19 = bitcast i8* %18 to i8**
  %20 = load i8*, i8** %19, align 8
  store i8* %20, i8** %4, align 8
  %21 = load i8*, i8** %4, align 8
  call void @free(i8* noundef %21) #5
  br label %22

22:                                               ; preds = %11
  %23 = load i32, i32* %3, align 4
  %24 = add nsw i32 %23, 1
  store i32 %24, i32* %3, align 4
  br label %5, !llvm.loop !6

25:                                               ; preds = %5
  %26 = load %struct.List*, %struct.List** %2, align 8
  %27 = getelementptr inbounds %struct.List, %struct.List* %26, i32 0, i32 2
  %28 = load i8*, i8** %27, align 8
  call void @free(i8* noundef %28) #5
  %29 = load %struct.List*, %struct.List** %2, align 8
  %30 = bitcast %struct.List* %29 to i8*
  call void @free(i8* noundef %30) #5
  ret void
}

; Function Attrs: nounwind
declare void @free(i8* noundef) #1

; Function Attrs: noinline nounwind optnone uwtable
define dso_local void @Vector_grow(%struct.List* noundef %0) #0 {
  %2 = alloca %struct.List*, align 8
  %3 = alloca i8*, align 8
  %4 = alloca i8*, align 8
  store %struct.List* %0, %struct.List** %2, align 8
  %5 = load %struct.List*, %struct.List** %2, align 8
  %6 = getelementptr inbounds %struct.List, %struct.List* %5, i32 0, i32 1
  %7 = load i32, i32* %6, align 4
  %8 = add nsw i32 %7, 10
  %9 = load %struct.List*, %struct.List** %2, align 8
  %10 = getelementptr inbounds %struct.List, %struct.List* %9, i32 0, i32 1
  store i32 %8, i32* %10, align 4
  %11 = load %struct.List*, %struct.List** %2, align 8
  %12 = getelementptr inbounds %struct.List, %struct.List* %11, i32 0, i32 1
  %13 = load i32, i32* %12, align 4
  %14 = sext i32 %13 to i64
  %15 = mul i64 %14, 8
  %16 = call noalias i8* @malloc(i64 noundef %15) #5
  store i8* %16, i8** %3, align 8
  %17 = load i8*, i8** %3, align 8
  %18 = load %struct.List*, %struct.List** %2, align 8
  %19 = getelementptr inbounds %struct.List, %struct.List* %18, i32 0, i32 2
  %20 = load i8*, i8** %19, align 8
  %21 = load %struct.List*, %struct.List** %2, align 8
  %22 = getelementptr inbounds %struct.List, %struct.List* %21, i32 0, i32 0
  %23 = load i32, i32* %22, align 8
  %24 = sext i32 %23 to i64
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %17, i8* align 1 %20, i64 %24, i1 false)
  store i8* %17, i8** %4, align 8
  %25 = load %struct.List*, %struct.List** %2, align 8
  %26 = getelementptr inbounds %struct.List, %struct.List* %25, i32 0, i32 2
  %27 = load i8*, i8** %26, align 8
  call void @free(i8* noundef %27) #5
  %28 = load i8*, i8** %4, align 8
  %29 = load %struct.List*, %struct.List** %2, align 8
  %30 = getelementptr inbounds %struct.List, %struct.List* %29, i32 0, i32 2
  store i8* %28, i8** %30, align 8
  ret void
}

; Function Attrs: argmemonly nofree nounwind willreturn
declare void @llvm.memcpy.p0i8.p0i8.i64(i8* noalias nocapture writeonly, i8* noalias nocapture readonly, i64, i1 immarg) #3

; Function Attrs: noinline nounwind optnone uwtable
define dso_local void @Vector_shrink(%struct.List* noundef %0) #0 {
  %2 = alloca %struct.List*, align 8
  store %struct.List* %0, %struct.List** %2, align 8
  %3 = load %struct.List*, %struct.List** %2, align 8
  %4 = getelementptr inbounds %struct.List, %struct.List* %3, i32 0, i32 1
  %5 = load i32, i32* %4, align 4
  %6 = sdiv i32 %5, 2
  %7 = load %struct.List*, %struct.List** %2, align 8
  %8 = getelementptr inbounds %struct.List, %struct.List* %7, i32 0, i32 1
  store i32 %6, i32* %8, align 4
  %9 = load %struct.List*, %struct.List** %2, align 8
  %10 = getelementptr inbounds %struct.List, %struct.List* %9, i32 0, i32 2
  %11 = load i8*, i8** %10, align 8
  %12 = load %struct.List*, %struct.List** %2, align 8
  %13 = getelementptr inbounds %struct.List, %struct.List* %12, i32 0, i32 1
  %14 = load i32, i32* %13, align 4
  %15 = sext i32 %14 to i64
  %16 = call i8* @realloc(i8* noundef %11, i64 noundef %15) #5
  %17 = load %struct.List*, %struct.List** %2, align 8
  %18 = getelementptr inbounds %struct.List, %struct.List* %17, i32 0, i32 2
  store i8* %16, i8** %18, align 8
  ret void
}

; Function Attrs: nounwind
declare i8* @realloc(i8* noundef, i64 noundef) #1

; Function Attrs: noinline nounwind optnone uwtable
define dso_local void @Vector_push(%struct.List* noundef %0, i8* noundef %1) #0 {
  %3 = alloca %struct.List*, align 8
  %4 = alloca i8*, align 8
  %5 = alloca i8*, align 8
  store %struct.List* %0, %struct.List** %3, align 8
  store i8* %1, i8** %4, align 8
  %6 = load %struct.List*, %struct.List** %3, align 8
  %7 = getelementptr inbounds %struct.List, %struct.List* %6, i32 0, i32 0
  %8 = load i32, i32* %7, align 8
  %9 = add nsw i32 %8, 1
  %10 = load %struct.List*, %struct.List** %3, align 8
  %11 = getelementptr inbounds %struct.List, %struct.List* %10, i32 0, i32 1
  %12 = load i32, i32* %11, align 4
  %13 = icmp sge i32 %9, %12
  br i1 %13, label %14, label %16

14:                                               ; preds = %2
  %15 = load %struct.List*, %struct.List** %3, align 8
  call void @Vector_grow(%struct.List* noundef %15)
  br label %16

16:                                               ; preds = %14, %2
  %17 = load %struct.List*, %struct.List** %3, align 8
  %18 = getelementptr inbounds %struct.List, %struct.List* %17, i32 0, i32 2
  %19 = load i8*, i8** %18, align 8
  %20 = load %struct.List*, %struct.List** %3, align 8
  %21 = getelementptr inbounds %struct.List, %struct.List* %20, i32 0, i32 0
  %22 = load i32, i32* %21, align 8
  %23 = sext i32 %22 to i64
  %24 = mul i64 %23, 8
  %25 = getelementptr i8, i8* %19, i64 %24
  store i8* %25, i8** %5, align 8
  %26 = load i8*, i8** %5, align 8
  %27 = bitcast i8** %4 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %26, i8* align 8 %27, i64 8, i1 false)
  store i8* %26, i8** %5, align 8
  %28 = load %struct.List*, %struct.List** %3, align 8
  %29 = getelementptr inbounds %struct.List, %struct.List* %28, i32 0, i32 0
  %30 = load i32, i32* %29, align 8
  %31 = add nsw i32 %30, 1
  store i32 %31, i32* %29, align 8
  ret void
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i8* @Vector_pop(%struct.List* noundef %0) #0 {
  %2 = alloca %struct.List*, align 8
  %3 = alloca i8*, align 8
  store %struct.List* %0, %struct.List** %2, align 8
  %4 = load %struct.List*, %struct.List** %2, align 8
  %5 = getelementptr inbounds %struct.List, %struct.List* %4, i32 0, i32 0
  %6 = load i32, i32* %5, align 8
  %7 = icmp sgt i32 %6, 0
  br i1 %7, label %8, label %9

8:                                                ; preds = %1
  br label %10

9:                                                ; preds = %1
  call void @__assert_fail(i8* noundef getelementptr inbounds ([14 x i8], [14 x i8]* @.str.2, i64 0, i64 0), i8* noundef getelementptr inbounds ([7 x i8], [7 x i8]* @.str.1, i64 0, i64 0), i32 noundef 63, i8* noundef getelementptr inbounds ([32 x i8], [32 x i8]* @__PRETTY_FUNCTION__.Vector_pop, i64 0, i64 0)) #6
  unreachable

10:                                               ; preds = %8
  %11 = load %struct.List*, %struct.List** %2, align 8
  %12 = getelementptr inbounds %struct.List, %struct.List* %11, i32 0, i32 2
  %13 = load i8*, i8** %12, align 8
  %14 = load %struct.List*, %struct.List** %2, align 8
  %15 = getelementptr inbounds %struct.List, %struct.List* %14, i32 0, i32 0
  %16 = load i32, i32* %15, align 8
  %17 = add nsw i32 %16, 1
  %18 = sext i32 %17 to i64
  %19 = mul i64 %18, 8
  %20 = getelementptr i8, i8* %13, i64 %19
  store i8* %20, i8** %3, align 8
  %21 = load %struct.List*, %struct.List** %2, align 8
  %22 = getelementptr inbounds %struct.List, %struct.List* %21, i32 0, i32 0
  %23 = load i32, i32* %22, align 8
  %24 = add nsw i32 %23, -1
  store i32 %24, i32* %22, align 8
  %25 = load %struct.List*, %struct.List** %2, align 8
  %26 = getelementptr inbounds %struct.List, %struct.List* %25, i32 0, i32 0
  %27 = load i32, i32* %26, align 8
  %28 = load %struct.List*, %struct.List** %2, align 8
  %29 = getelementptr inbounds %struct.List, %struct.List* %28, i32 0, i32 1
  %30 = load i32, i32* %29, align 4
  %31 = sdiv i32 %30, 2
  %32 = icmp slt i32 %27, %31
  br i1 %32, label %33, label %35

33:                                               ; preds = %10
  %34 = load %struct.List*, %struct.List** %2, align 8
  call void @Vector_shrink(%struct.List* noundef %34)
  br label %35

35:                                               ; preds = %33, %10
  %36 = load i8*, i8** %3, align 8
  ret i8* %36
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @main() #0 {
  %1 = alloca i32, align 4
  %2 = alloca %struct.List*, align 8
  %3 = alloca i32*, align 8
  %4 = alloca i32*, align 8
  %5 = alloca i32*, align 8
  store i32 0, i32* %1, align 4
  %6 = call %struct.List* @Vector_alloc()
  store %struct.List* %6, %struct.List** %2, align 8
  %7 = call noalias i8* @malloc(i64 noundef 4) #5
  %8 = bitcast i8* %7 to i32*
  store i32* %8, i32** %3, align 8
  %9 = load i32*, i32** %3, align 8
  store i32 26, i32* %9, align 4
  %10 = call noalias i8* @malloc(i64 noundef 4) #5
  %11 = bitcast i8* %10 to i32*
  store i32* %11, i32** %4, align 8
  %12 = load i32*, i32** %4, align 8
  store i32 1738, i32* %12, align 4
  %13 = load %struct.List*, %struct.List** %2, align 8
  %14 = load i32*, i32** %3, align 8
  %15 = bitcast i32* %14 to i8*
  call void @Vector_push(%struct.List* noundef %13, i8* noundef %15)
  %16 = load %struct.List*, %struct.List** %2, align 8
  %17 = load i32*, i32** %4, align 8
  %18 = bitcast i32* %17 to i8*
  call void @Vector_push(%struct.List* noundef %16, i8* noundef %18)
  %19 = load %struct.List*, %struct.List** %2, align 8
  %20 = call i8* @Vector_get(%struct.List* noundef %19, i32 noundef 0)
  %21 = bitcast i8* %20 to i32*
  store i32* %21, i32** %5, align 8
  %22 = load i32*, i32** %5, align 8
  %23 = load i32, i32* %22, align 4
  %24 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([18 x i8], [18 x i8]* @.str.3, i64 0, i64 0), i32 noundef %23)
  %25 = load %struct.List*, %struct.List** %2, align 8
  call void @Vector_free(%struct.List* noundef %25)
  ret i32 0
}

declare i32 @printf(i8* noundef, ...) #4

attributes #0 = { noinline nounwind optnone uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nounwind "frame-pointer"="all" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #2 = { noreturn nounwind "frame-pointer"="all" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #3 = { argmemonly nofree nounwind willreturn }
attributes #4 = { "frame-pointer"="all" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #5 = { nounwind }
attributes #6 = { noreturn nounwind }

!llvm.module.flags = !{!0, !1, !2, !3, !4}
!llvm.ident = !{!5}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 7, !"PIC Level", i32 2}
!2 = !{i32 7, !"PIE Level", i32 2}
!3 = !{i32 7, !"uwtable", i32 1}
!4 = !{i32 7, !"frame-pointer", i32 2}
!5 = !{!"Ubuntu clang version 14.0.6"}
!6 = distinct !{!6, !7}
!7 = !{!"llvm.loop.mustprogress"}
